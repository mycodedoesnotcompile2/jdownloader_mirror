/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.IO;
import org.appwork.utils.os.windows.execute.RunAsTokenEnvironment;
import org.appwork.utils.os.windows.execute.RunAsWin32ApiTrace;
import org.appwork.utils.os.windows.execute.jna.Advapi32CreateProcessLib;
import org.appwork.utils.os.windows.execute.jna.Advapi32CreateProcessWithLogonWLib;
import org.appwork.utils.os.windows.execute.jna.Advapi32DuplicateTokenExLib;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * Starts {@link RunSerializedTaskMain} (or any command line) under the given credentials in the caller's WTS session. Prefers
 * {@code LogonUser} + {@code CreateProcessAsUserW} with the interactive logon token (typically filtered for admin-group users). Falls back
 * to {@code CreateProcessWithLogonW} when {@code LogonUser} or {@code CreateProcessAsUserW} fails. UAC elevation for another user is not
 * done here; use a separate {@code runas} / {@link org.appwork.testframework.executer.AdminExecuter#runAsAdmin} step when needed.
 * <p>
 * Process creation uses {@link RunAsTokenEnvironment} ({@code CreateEnvironmentBlock}, {@code bInherit=false}) so the child does not inherit
 * the elevated helper JVM environment.
 */
final class RunTaskAsUserLauncher {
    private static final int TOKEN_TYPE_PRIMARY = 1;
    private static final AtomicLong CMD_WRAPPER_ID = new AtomicLong(System.currentTimeMillis());
    /** Win32 {@code ERROR_INVALID_PARAMETER} — often seen when the command line is too long for {@code CreateProcessWithLogonW}. */
    private static final int WIN32_ERROR_INVALID_PARAMETER = 87;
    /** Same mask as RunAsHelper primary duplicate for process creation. */
    private static final int DUPLICATE_PRIMARY_TOKEN_ACCESS = WinNT.TOKEN_ASSIGN_PRIMARY | WinNT.TOKEN_DUPLICATE | WinNT.TOKEN_QUERY | WinNT.TOKEN_ADJUST_DEFAULT | WinNT.TOKEN_ADJUST_SESSIONID | WinNT.TOKEN_IMPERSONATE;

    private RunTaskAsUserLauncher() {
    }

    /**
     * Creates the process and returns the process handle (caller must wait and close). Thread handle in {@code pi} is closed.
     *
     * @param workingDir
     *            current directory for the new process
     * @param cmdTokens
     *            argv-style command (e.g. java.exe, -cp, ..., main class, args)
     */
    static HANDLE createProcessWithLogon(File workingDir, String[] cmdTokens, String domain, String user, String password) throws IOException {
        if (user == null || user.trim().length() == 0) {
            throw new IllegalArgumentException("user is required");
        }
        if (password == null) {
            throw new IllegalArgumentException("password is required");
        }
        String workDir = workingDir != null ? workingDir.getAbsolutePath() : null;
        String domainArg = domain != null && domain.trim().length() > 0 ? domain.trim() : null;
        LogV3.info("Run in " + workDir);
        final HANDLEByReference phLogon = new HANDLEByReference();
        RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "LogonUser", "user=" + user.trim() + " domain=" + (domainArg != null ? domainArg : "(null)") + " type=LOGON32_LOGON_INTERACTIVE");
        if (!Advapi32.INSTANCE.LogonUser(user.trim(), domainArg, password, WinBase.LOGON32_LOGON_INTERACTIVE, WinBase.LOGON32_PROVIDER_DEFAULT, phLogon)) {
            final int logonErr = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "LogonUser", false, logonErr);
            LogV3.info("RunTaskAsUserLauncher: LogonUser failed err=" + logonErr + " (" + new Win32Exception(logonErr).getMessage() + "), using CreateProcessWithLogonW");
            LogV3.info("RunTaskAsUserLauncher: starting subprocess via CreateProcessWithLogonW user=" + user.trim());
            return createProcessWithLogonWOnly(workDir, cmdTokens, domainArg, user, password, null);
        }
        int gleLogonOk = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "LogonUser", true, gleLogonOk);
        final HANDLE hLogon = phLogon.getValue();
        HANDLE hPrimary = null;
        Pointer envBlock = null;
        try {
            LogV3.info("RunTaskAsUserLauncher: LogonUser ok user=" + user.trim());
            try {
                envBlock = RunAsTokenEnvironment.createForPrimaryToken(hLogon, false);
            } catch (Win32Exception envEx) {
                LogV3.info("RunTaskAsUserLauncher: CreateEnvironmentBlock after LogonUser failed (" + envEx.getMessage() + "), subprocess may inherit caller env on CPWL fallback");
            }
            try {
                hPrimary = duplicateToPrimaryTokenWithFallbacks(hLogon, "LogonUserPath");
            } catch (Win32Exception dupEx) {
                LogV3.info("RunTaskAsUserLauncher: DuplicateTokenEx after LogonUser failed (" + dupEx.getMessage() + "), falling back to CreateProcessWithLogonW");
            }
            if (hPrimary != null) {
                try {
                    LogV3.info("RunTaskAsUserLauncher: starting subprocess via CreateProcessAsUserW (LogonUser path) user=" + user.trim());
                    return createProcessAsUserWithPrimaryToken(workDir, cmdTokens, hPrimary);
                } catch (IOException cpauEx) {
                    LogV3.info("RunTaskAsUserLauncher: CreateProcessAsUserW after LogonUser failed (" + cpauEx.getMessage() + "), falling back to CreateProcessWithLogonW");
                    try {
                        RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "hPrimary after CPAU failure");
                        boolean clp = Kernel32.INSTANCE.CloseHandle(hPrimary);
                        RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(hPrimary)", clp, Kernel32.INSTANCE.GetLastError());
                    } catch (Throwable closePrimary) {
                        LogV3.log(closePrimary);
                    }
                    hPrimary = null;
                }
            }
            LogV3.info("RunTaskAsUserLauncher: starting subprocess via CreateProcessWithLogonW user=" + user.trim());
            return createProcessWithLogonWOnly(workDir, cmdTokens, domainArg, user, password, envBlock);
        } finally {
            RunAsTokenEnvironment.destroy(envBlock);
            if (hLogon != null) {
                try {
                    RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "hLogon");
                    boolean cl = Kernel32.INSTANCE.CloseHandle(hLogon);
                    RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(hLogon)", cl, Kernel32.INSTANCE.GetLastError());
                } catch (Throwable t) {
                    LogV3.log(t);
                }
            }
        }
    }

    /**
     * Tries several {@code DuplicateTokenEx} access / impersonation-level combinations ({@code LogonUser} tokens vary by OS and policy; Win32
     * 1309 is common for a mismatched level). Order matches {@link org.appwork.utils.os.windows.execute.RunAsHelper}: try
     * {@code SecurityImpersonation} early, then other levels; {@code dwDesiredAccess} includes 0, full process-creation mask,
     * {@code TOKEN_ALL_ACCESS}, and {@code TOKEN_MAXIMUM_ALLOWED} (0x02000000).
     */
    private static HANDLE duplicateToPrimaryTokenWithFallbacks(final HANDLE source, final String contextTag) {
        final int[] impersonationLevels = new int[] { WinNT.SECURITY_IMPERSONATION_LEVEL.SecurityImpersonation, WinNT.SECURITY_IMPERSONATION_LEVEL.SecurityDelegation, WinNT.SECURITY_IMPERSONATION_LEVEL.SecurityIdentification, WinNT.SECURITY_IMPERSONATION_LEVEL.SecurityAnonymous };
        /** TOKEN_MAXIMUM_ALLOWED — ask kernel for maximum rights the caller may duplicate. */
        final int tokenMaximumAllowed = 0x02000000;
        final int[] desiredAccesses = new int[] { 0, DUPLICATE_PRIMARY_TOKEN_ACCESS, WinNT.TOKEN_ALL_ACCESS, tokenMaximumAllowed };
        int lastGle = -1;
        for (int ai = 0; ai < desiredAccesses.length; ai++) {
            final int desiredAccess = desiredAccesses[ai];
            for (int li = 0; li < impersonationLevels.length; li++) {
                final int impLevel = impersonationLevels[li];
                HANDLEByReference pPrimary = new HANDLEByReference();
                RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "DuplicateTokenEx", contextTag + " source=" + RunAsWin32ApiTrace.h(source) + " desiredAccess=0x" + Integer.toHexString(desiredAccess) + " impLevel=" + impLevel + " type=Primary");
                final boolean ok = Advapi32DuplicateTokenExLib.INSTANCE.DuplicateTokenEx(source, desiredAccess, null, impLevel, TOKEN_TYPE_PRIMARY, pPrimary);
                final int gle = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "DuplicateTokenEx", ok, gle);
                if (ok) {
                    LogV3.info("RunTaskAsUserLauncher: DuplicateTokenEx succeeded (" + contextTag + ") desiredAccess=0x" + Integer.toHexString(desiredAccess) + " impLevel=" + impLevel);
                    return pPrimary.getValue();
                }
                lastGle = gle;
            }
        }
        throw new Win32Exception(lastGle);
    }

    private static HANDLE createProcessAsUserWithPrimaryToken(String workDir, String[] cmdTokens, HANDLE hPrimary) throws IOException {
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmdTokens);
        int cmdLineBufSize = Native.WCHAR_SIZE * (commandLine.length() + 1);
        Memory cmdLineMem = new Memory(cmdLineBufSize);
        cmdLineMem.setWideString(0, commandLine);
        WinBase.STARTUPINFO si = new WinBase.STARTUPINFO();
        si.clear();
        si.cb = new DWORD(si.size());
        WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
        pi.clear();
        int creationFlags = Kernel32.CREATE_UNICODE_ENVIRONMENT | WinBase.CREATE_NO_WINDOW;
        Pointer envBlock = null;
        boolean ok = false;
        int errC = 0;
        try {
            envBlock = RunAsTokenEnvironment.createForPrimaryToken(hPrimary, false);
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CreateProcessAsUserW", "token=" + RunAsWin32ApiTrace.h(hPrimary) + " workDir=" + workDir + " flags=0x" + Integer.toHexString(creationFlags) + " lpEnvironment=userProfileBlock");
            ok = Advapi32CreateProcessLib.INSTANCE.CreateProcessAsUser(hPrimary, Pointer.NULL, cmdLineMem, null, null, true, creationFlags, envBlock, workDir, si, pi);
            errC = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CreateProcessAsUserW", ok, errC);
        } finally {
            RunAsTokenEnvironment.destroy(envBlock);
        }
        if (!ok) {
            try {
                Kernel32.INSTANCE.CloseHandle(hPrimary);
            } catch (Throwable ignore) {
            }
            throw new IOException("CreateProcessAsUserW failed: Win32 error " + errC + " (" + new Win32Exception(errC).getMessage() + ") in " + workDir);
        }
        try {
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "hPrimary after successful CPAU");
            boolean clt = Kernel32.INSTANCE.CloseHandle(hPrimary);
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(hPrimary)", clt, Kernel32.INSTANCE.GetLastError());
        } catch (Throwable t) {
            LogV3.log(t);
        }
        if (pi.hThread != null && Pointer.nativeValue(pi.hThread.getPointer()) != 0) {
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "pi.hThread");
            boolean cth = Kernel32.INSTANCE.CloseHandle(pi.hThread);
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(pi.hThread)", cth, Kernel32.INSTANCE.GetLastError());
        }
        return pi.hProcess;
    }

    /**
     * @param envBlock
     *            optional environment block for the target user; destroyed by caller when non-null and this method returns successfully only
     *            if caller passed it — when built locally, this method destroys it in {@code finally}
     */
    private static HANDLE createProcessWithLogonWOnly(String workDir, String[] cmdTokens, String domainArg, String user, String password, Pointer envBlock) throws IOException {
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmdTokens);
        final int originalCommandLineLength = commandLine != null ? commandLine.length() : -1;
        final String workDirInfo;
        if (workDir == null) {
            workDirInfo = "null";
        } else {
            final File wd = new File(workDir);
            workDirInfo = workDir + " (exists=" + wd.exists() + ", dir=" + wd.isDirectory() + ")";
        }
        // CreateProcessWithLogonW is much more sensitive to long command lines than CreateProcessAsUser.
        // With large Java classpaths, route through a short cmd wrapper so lpCommandLine stays small.
        if (commandLine != null && commandLine.length() > 1000) {
            if (workDir == null || !new File(workDir).isDirectory()) {
                throw new IOException("CreateProcessWithLogonW long command line workaround requires a valid workDir directory, got: " + workDirInfo + ", commandLineLength=" + originalCommandLineLength);
            }
            final File wrapperFile = new File(workDir, "runas_user_wrapper_" + CMD_WRAPPER_ID.incrementAndGet() + ".cmd");
            final String script = "@echo off\r\n" + commandLine + "\r\n";
            IO.writeStringToFile(wrapperFile, script, false, null);
            commandLine = "cmd.exe /c \"" + wrapperFile.getAbsolutePath() + "\"";
        }
        final int commandLineLength = commandLine != null ? commandLine.length() : -1;
        int cmdLineBufSize = Native.WCHAR_SIZE * (commandLine.length() + 1);
        Memory cmdLineMem = new Memory(cmdLineBufSize);
        cmdLineMem.setWideString(0, commandLine);
        WinBase.STARTUPINFO si = new WinBase.STARTUPINFO();
        si.clear();
        si.cb = new DWORD(si.size());
        WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
        pi.clear();
        int creationFlags = Kernel32.CREATE_UNICODE_ENVIRONMENT | WinBase.CREATE_NO_WINDOW;
        Pointer env = envBlock;
        boolean ownEnv = false;
        HANDLE hLogonForEnv = null;
        try {
            if (env == null) {
                final HANDLEByReference phLogon = new HANDLEByReference();
                RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "LogonUser", "CPWL env block user=" + user.trim());
                if (Advapi32.INSTANCE.LogonUser(user.trim(), domainArg, password, WinBase.LOGON32_LOGON_INTERACTIVE, WinBase.LOGON32_PROVIDER_DEFAULT, phLogon)) {
                    RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "LogonUser(CPWL env)", true, Kernel32.INSTANCE.GetLastError());
                    hLogonForEnv = phLogon.getValue();
                    try {
                        env = RunAsTokenEnvironment.createForPrimaryToken(hLogonForEnv, false);
                        ownEnv = true;
                    } catch (Win32Exception envEx) {
                        LogV3.info("RunTaskAsUserLauncher: CreateEnvironmentBlock for CPWL failed (" + envEx.getMessage() + "), lpEnvironment=null");
                    }
                } else {
                    RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "LogonUser(CPWL env)", false, Kernel32.INSTANCE.GetLastError());
                }
            }
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CreateProcessWithLogonW", "user=" + user.trim() + " workDir=" + workDir + " lpEnvironment=" + (env != null ? "userProfileBlock" : "null"));
            final boolean ok = Advapi32CreateProcessWithLogonWLib.INSTANCE.CreateProcessWithLogonW(user.trim(), domainArg, password, Advapi32CreateProcessWithLogonWLib.LOGON_WITH_PROFILE, null, cmdLineMem, creationFlags, env, workDir, si, pi);
            final int errL = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CreateProcessWithLogonW", ok, errL);
            if (!ok) {
            final String cmdPreview = commandLineLength > 512 ? commandLine.substring(0, 512) + "...(truncated)" : commandLine;
            final String cpwlHint = errL == WIN32_ERROR_INVALID_PARAMETER ? " If the command line is long, CreateProcessWithLogonW often fails with this error; shorten the classpath or paths." : "";
                throw new IOException("CreateProcessWithLogonW failed: Win32 error " + errL + " (" + new Win32Exception(errL).getMessage() + "), user=" + user.trim() + ", domain=" + (domainArg != null ? domainArg : "(null)") + ", workDir=" + workDirInfo + ", commandLineLength=" + commandLineLength + ", originalCommandLineLength=" + originalCommandLineLength + ", commandLinePreview=" + cmdPreview + "." + cpwlHint);
            }
            if (pi.hThread != null && Pointer.nativeValue(pi.hThread.getPointer()) != 0) {
                RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "pi.hThread CPWL");
                boolean cth = Kernel32.INSTANCE.CloseHandle(pi.hThread);
                RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(pi.hThread)", cth, Kernel32.INSTANCE.GetLastError());
            }
            return pi.hProcess;
        } finally {
            if (ownEnv) {
                RunAsTokenEnvironment.destroy(env);
            }
            if (hLogonForEnv != null) {
                try {
                    Kernel32.INSTANCE.CloseHandle(hLogonForEnv);
                } catch (Throwable t) {
                    LogV3.log(t);
                }
            }
        }
    }

    /**
     * Waits for process exit, returns exit code, closes the handle.
     */
    static int waitForExitAndClose(HANDLE hProcess) {
        if (hProcess == null) {
            return -1;
        }
        try {
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "WaitForSingleObject", "hProcess=" + RunAsWin32ApiTrace.h(hProcess) + " timeout=INFINITE");
            int wrc = Kernel32.INSTANCE.WaitForSingleObject(hProcess, Kernel32.INFINITE);
            int gleW = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "WaitForSingleObject", true, gleW);
            if (RunAsWin32ApiTrace.isEnabled()) {
                LogV3.info(RunAsWin32ApiTrace.PREFIX + " RunTaskAsUserLauncher INFO WaitForSingleObject returnCode=" + wrc);
            }
            IntByReference exitCodeRef = new IntByReference();
            RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "GetExitCodeProcess", RunAsWin32ApiTrace.h(hProcess));
            if (!Kernel32.INSTANCE.GetExitCodeProcess(hProcess, exitCodeRef)) {
                int gleG = Kernel32.INSTANCE.GetLastError();
                RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "GetExitCodeProcess", false, gleG);
                LogV3.warning("RunTaskAsUserLauncher: GetExitCodeProcess failed");
                return -1;
            }
            RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "GetExitCodeProcess", true, Kernel32.INSTANCE.GetLastError());
            return exitCodeRef.getValue();
        } finally {
            try {
                RunAsWin32ApiTrace.in("RunTaskAsUserLauncher", "CloseHandle", "hProcess waitForExitAndClose");
                boolean cl = Kernel32.INSTANCE.CloseHandle(hProcess);
                RunAsWin32ApiTrace.out("RunTaskAsUserLauncher", "CloseHandle(hProcess)", cl, Kernel32.INSTANCE.GetLastError());
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }
}
