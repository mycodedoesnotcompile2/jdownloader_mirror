/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.LogCallback;
import org.appwork.utils.os.windows.execute.jna.Advapi32CreateProcessLib;
import org.appwork.utils.os.windows.execute.jna.Advapi32CreateProcessWithTokenLib;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * Starts a child with {@code CreateProcessAsUser} / {@code CreateProcessWithTokenW} using an already-prepared primary token. Builds
 * {@code lpEnvironment} via {@link RunAsTokenEnvironment} ({@code CreateEnvironmentBlock}, {@code bInherit=false}) so the caller process
 * environment (e.g. LocalSystem) is not passed to the child. {@link RunAsTokenWorkingDirectory} validates optional caller
 * {@link RunAsLaunchOptions#getWorkingDir()} for the target token. Does not take ownership of {@code primaryToken}.
 */
final class RunAsProcessLauncher {
    private static void debug(String msg) {
        LogV3.info("RunAsLauncherDebug: " + msg);
    }


    private static final int STARTF_USESTDHANDLES = 0x00000100;

    private RunAsProcessLauncher() {
    }

    static ProcessOutput runWithPrimaryToken(HANDLE primaryToken, String[] cmd, RunAsLaunchOptions options) throws Exception {
        return runWithToken(primaryToken, cmd, options, false);
    }

    static ProcessOutput runWithPrimaryTokenUsingCreateProcessWithToken(HANDLE primaryToken, String[] cmd, RunAsLaunchOptions options) throws Exception {
        return runWithToken(primaryToken, cmd, options, true, Advapi32CreateProcessWithTokenLib.LOGON_WITH_PROFILE);
    }

    /**
     * Same as {@link #runWithPrimaryTokenUsingCreateProcessWithToken(HANDLE, String[], RunAsLaunchOptions)} with explicit
     * {@code CreateProcessWithTokenW} {@code dwLogonFlags} (0 = no profile load).
     */
    static ProcessOutput runWithPrimaryTokenUsingCreateProcessWithToken(HANDLE primaryToken, String[] cmd, RunAsLaunchOptions options, int logonFlags) throws Exception {
        return runWithToken(primaryToken, cmd, options, true, logonFlags);
    }

    private static ProcessOutput runWithToken(HANDLE primaryToken, String[] cmd, RunAsLaunchOptions options, boolean createProcessWithToken) throws Exception {
        return runWithToken(primaryToken, cmd, options, createProcessWithToken, Advapi32CreateProcessWithTokenLib.LOGON_WITH_PROFILE);
    }

    private static ProcessOutput runWithToken(HANDLE primaryToken, String[] cmd, RunAsLaunchOptions options, boolean createProcessWithToken, int logonFlags) throws Exception {
        File workingDir = options.getWorkingDir();
        boolean waitFor = options.isWaitFor();
        final String workDirPath = RunAsTokenWorkingDirectory.resolveForPrimaryToken(primaryToken, workingDir);
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        debug("launchApi=" + (createProcessWithToken ? "CreateProcessWithTokenW(logonFlags=0x" + Integer.toHexString(logonFlags) + ")" : "CreateProcessAsUserW") + " commandLine=\""
                + (commandLine.length() > 200 ? commandLine.substring(0, 200) + "..." : commandLine) + "\" workDirPath=" + workDirPath + " waitFor=" + waitFor);
        int cmdLineBufSize = Native.WCHAR_SIZE * (commandLine.length() + 1);
        Memory cmdLineMem = new Memory(cmdLineBufSize);
        cmdLineMem.setWideString(0, commandLine);
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
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CreatePipe", "stdout");
                boolean pipe1 = Kernel32.INSTANCE.CreatePipe(hStdOutRdRef, hStdOutWrRef, sa, 0);
                int gle1 = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CreatePipe(stdout)", pipe1, gle1);
                if (!pipe1) {
                    throw new Win32Exception(gle1);
                }
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CreatePipe", "stderr");
                boolean pipe2 = Kernel32.INSTANCE.CreatePipe(hStdErrRdRef, hStdErrWrRef, sa, 0);
                int gle2 = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CreatePipe(stderr)", pipe2, gle2);
                if (!pipe2) {
                    closeHandleSafe(hStdOutRdRef.getValue());
                    closeHandleSafe(hStdOutWrRef.getValue());
                    throw new Win32Exception(gle2);
                }
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CreatePipe", "stdin");
                boolean pipe3 = Kernel32.INSTANCE.CreatePipe(hStdInRdRef, hStdInWrRef, sa, 0);
                int gle3 = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CreatePipe(stdin)", pipe3, gle3);
                if (!pipe3) {
                    closeHandleSafe(hStdOutRdRef.getValue());
                    closeHandleSafe(hStdOutWrRef.getValue());
                    closeHandleSafe(hStdErrRdRef.getValue());
                    closeHandleSafe(hStdErrWrRef.getValue());
                    throw new Win32Exception(gle3);
                }
                hStdOutRd = hStdOutRdRef.getValue();
                hStdErrRd = hStdErrRdRef.getValue();
                hStdInRd = hStdInRdRef.getValue();
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
            WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
            pi.clear();
            int creationFlags = Kernel32.CREATE_UNICODE_ENVIRONMENT | WinBase.CREATE_NO_WINDOW;
            final String apiName = createProcessWithToken ? "CreateProcessWithTokenW" : "CreateProcessAsUserW";
            // null lpEnvironment inherits the caller block (LocalSystem / elevated admin vars leak into the session-owner child).
            Pointer envBlock = null;
            boolean ok = false;
            int createProcessLastError = 0;
            try {
                envBlock = RunAsTokenEnvironment.createForPrimaryToken(primaryToken, false);
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", apiName, "token=" + RunAsWin32ApiTrace.h(primaryToken) + " creationFlags=0x" + Integer.toHexString(creationFlags) + " workDir=" + workDirPath + " lpEnvironment=userProfileBlock");
                ok = createProcessWithToken ? Advapi32CreateProcessWithTokenLib.INSTANCE.CreateProcessWithTokenW(primaryToken, logonFlags, null, new com.sun.jna.WString(commandLine), creationFlags, envBlock, workDirPath, si, pi)
                        : Advapi32CreateProcessLib.INSTANCE.CreateProcessAsUser(primaryToken, Pointer.NULL, cmdLineMem, null, null, true, creationFlags, envBlock, workDirPath, si, pi);
                createProcessLastError = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", apiName, ok, createProcessLastError);
            } finally {
                RunAsTokenEnvironment.destroy(envBlock);
            }
            if (waitFor) {
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "hStdInWr");
                boolean c1 = Kernel32.INSTANCE.CloseHandle(hStdInWrRef.getValue());
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(hStdInWr)", c1, Kernel32.INSTANCE.GetLastError());
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "hStdOutWr");
                boolean c2 = Kernel32.INSTANCE.CloseHandle(hStdOutWrRef.getValue());
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(hStdOutWr)", c2, Kernel32.INSTANCE.GetLastError());
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "hStdErrWr");
                boolean c3 = Kernel32.INSTANCE.CloseHandle(hStdErrWrRef.getValue());
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(hStdErrWr)", c3, Kernel32.INSTANCE.GetLastError());
            }
            if (!ok) {
                if (waitFor) {
                    closeHandleSafe(hStdInRd);
                    closeHandleSafe(hStdOutRd);
                    closeHandleSafe(hStdErrRd);
                }
                int err = (createProcessLastError != 0) ? createProcessLastError : 0x1F;
                String api = createProcessWithToken ? "CreateProcessWithTokenW" : "CreateProcessAsUserW";
                String errMsg = "";
                try {
                    errMsg = new Win32Exception(err).getMessage();
                } catch (Throwable ignore) {
                }
                int currentSession = -1;
                try {
                    RunAsWin32ApiTrace.in("RunAsProcessLauncher", "GetCurrentProcessId", "");
                    int curPid = Kernel32.INSTANCE.GetCurrentProcessId();
                    int glePid = Kernel32.INSTANCE.GetLastError();
                    RunAsWin32ApiTrace.out("RunAsProcessLauncher", "GetCurrentProcessId", true, glePid);
                    IntByReference sessionRef = new IntByReference();
                    RunAsWin32ApiTrace.in("RunAsProcessLauncher", "ProcessIdToSessionId", "pid=" + curPid);
                    boolean pts = Kernel32.INSTANCE.ProcessIdToSessionId(curPid, sessionRef);
                    int glePts = Kernel32.INSTANCE.GetLastError();
                    RunAsWin32ApiTrace.out("RunAsProcessLauncher", "ProcessIdToSessionId", pts, glePts);
                    if (pts) {
                        currentSession = sessionRef.getValue();
                    }
                } catch (Throwable ignore) {
                }
                debug(api + " failed. err=" + err + " (" + errMsg + "), workDir=" + workDirPath + ", commandLength=" + commandLine.length() + ", waitFor=" + waitFor + ", tokenPtr=0x"
                        + Long.toHexString(primaryToken != null && primaryToken.getPointer() != null ? Pointer.nativeValue(primaryToken.getPointer()) : 0) + ", currentSession=" + currentSession
                        + ", ERROR_BAD_IMPERSONATION_LEVEL(1346)=" + (err == 1346)
                        + ", ERROR_PRIVILEGE_NOT_HELD(1314)=" + (err == WinError.ERROR_PRIVILEGE_NOT_HELD)
                        + ", ERROR_ACCESS_DENIED(5)=" + (err == WinError.ERROR_ACCESS_DENIED)
                        + ", logonFlags=0x" + Integer.toHexString(logonFlags));
                // Extra token dump on launch failure (helps diagnose linked-token / Safer issues)
                try {
                    RunAsHelper.logTokenDiagnosticsForLaunch(primaryToken, "launchFail_" + api + "_err" + err);
                } catch (Throwable ignore) {
                }
                throw new Win32Exception(err);
            }
            if (pi.hThread != null && Pointer.nativeValue(pi.hThread.getPointer()) != 0) {
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "pi.hThread");
                boolean cth = Kernel32.INSTANCE.CloseHandle(pi.hThread);
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(pi.hThread)", cth, Kernel32.INSTANCE.GetLastError());
            }
            if (waitFor) {
                ByteArrayOutputStream stdoutData = new ByteArrayOutputStream();
                ByteArrayOutputStream stderrData = new ByteArrayOutputStream();
                LogCallback logCb = options.getLogCallback();
                Thread outReader = readPipeInBackground(hStdOutRd, stdoutData, logCb, true);
                Thread errReader = readPipeInBackground(hStdErrRd, stderrData, logCb, false);
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "WaitForSingleObject", "hProcess=" + RunAsWin32ApiTrace.h(pi.hProcess) + " timeout=INFINITE");
                int waitRc = Kernel32.INSTANCE.WaitForSingleObject(pi.hProcess, Kernel32.INFINITE);
                int gleWait = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "WaitForSingleObject", true, gleWait);
                if (RunAsWin32ApiTrace.isEnabled()) {
                    LogV3.info("RunAsApiTrace RunAsProcessLauncher INFO WaitForSingleObject returnCode=" + waitRc + " gle=" + gleWait);
                }
                try {
                    outReader.join(5000);
                    errReader.join(5000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                IntByReference exitCodeRef = new IntByReference();
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "GetExitCodeProcess", "hProcess=" + RunAsWin32ApiTrace.h(pi.hProcess));
                boolean gec = Kernel32.INSTANCE.GetExitCodeProcess(pi.hProcess, exitCodeRef);
                int gleGec = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "GetExitCodeProcess", gec, gleGec);
                int exitCode = exitCodeRef.getValue();
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "pi.hProcess(after_wait)");
                boolean cpr = Kernel32.INSTANCE.CloseHandle(pi.hProcess);
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(pi.hProcess)", cpr, Kernel32.INSTANCE.GetLastError());
                String codePage = getConsoleCodepageSafe();
                return new ProcessOutput(exitCode, stdoutData, stderrData, codePage);
            } else {
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "GetProcessId", "hProcess=" + RunAsWin32ApiTrace.h(pi.hProcess));
                int remotePid = Kernel32.INSTANCE.GetProcessId(pi.hProcess);
                int gleGpid = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "GetProcessId", true, gleGpid);
                if (RunAsWin32ApiTrace.isEnabled()) {
                    LogV3.info("RunAsApiTrace RunAsProcessLauncher INFO GetProcessId pid=" + remotePid);
                }
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", "pi.hProcess(nobg)");
                boolean cpn = Kernel32.INSTANCE.CloseHandle(pi.hProcess);
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle(pi.hProcess)", cpn, Kernel32.INSTANCE.GetLastError());
                String codePage = getConsoleCodepageSafe();
                return new ProcessOutput(-1, new ByteArrayOutputStream(), new ByteArrayOutputStream(), codePage, Integer.valueOf(remotePid), null);
            }
        } finally {
            closeHandleSafe(hStdInRd);
            closeHandleSafe(hStdOutRd);
            closeHandleSafe(hStdErrRd);
        }
    }

    private static void closeHandleSafe(HANDLE h) {
        if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h) && Pointer.nativeValue(h.getPointer()) != 0) {
            try {
                RunAsWin32ApiTrace.in("RunAsProcessLauncher", "CloseHandle", RunAsWin32ApiTrace.h(h));
                boolean c = Kernel32.INSTANCE.CloseHandle(h);
                RunAsWin32ApiTrace.out("RunAsProcessLauncher", "CloseHandle", c, Kernel32.INSTANCE.GetLastError());
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
                boolean firstRead = true;
                while (true) {
                    if (firstRead) {
                        RunAsWin32ApiTrace.in("RunAsProcessLauncher", "ReadFile", "pipe=" + RunAsWin32ApiTrace.h(hPipe) + " (further ReadFile calls not logged)");
                        firstRead = false;
                    }
                    boolean rf = Kernel32.INSTANCE.ReadFile(hPipe, buf, buf.length, read, null);
                    int gleRf = Kernel32.INSTANCE.GetLastError();
                    if (!rf) {
                        RunAsWin32ApiTrace.out("RunAsProcessLauncher", "ReadFile", false, gleRf);
                        break;
                    }
                    int n = read.getValue();
                    RunAsWin32ApiTrace.out("RunAsProcessLauncher", "ReadFile", true, gleRf);
                    if (RunAsWin32ApiTrace.isEnabled()) {
                        LogV3.info(RunAsWin32ApiTrace.PREFIX + " RunAsProcessLauncher INFO ReadFile bytesRead=" + n);
                    }
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
        }, "RunAsHelper-PipeReader");
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
