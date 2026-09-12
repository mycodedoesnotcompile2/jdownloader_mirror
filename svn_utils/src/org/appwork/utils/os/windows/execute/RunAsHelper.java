/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.jna.Advapi32CreateProcessWithTokenLib;
import org.appwork.utils.os.windows.execute.jna.Advapi32DuplicateTokenExLib;
import org.appwork.utils.os.windows.execute.jna.Advapi32SaferLib;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Guid.GUID;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.Shell32;
import com.sun.jna.platform.win32.ShlObj;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinNT.PSIDByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Windows run-as helpers structured in three stages: (1) detect caller / launch constraints, (2) resolve the target interactive session
 * owner ({@link InteractiveSessionOwner}), (3) execute with {@link java.lang.ProcessBuilder} or {@code CreateProcessAsUser} as appropriate.
 * <p>
 * When the caller is <strong>not elevated</strong>, {@code waitFor} is true, and the resolved owner is
 * the current user in the current process session, a normal {@link java.lang.ProcessBuilder} is used — {@code CreateProcessAsUser} would
 * require privileges standard users do not have. For elevated->non-elevated same-user/session launches, this helper uses
 * {@code CreateProcessWithTokenW} directly instead of {@code CreateProcessAsUserW} because that path is more reliable for normal admin
 * tokens.
 * <p>
 * <strong>vs. {@code org.appwork.jna.windows.experimental.ide.DesktopSessionCheck}:</strong> that POC runs {@code CreateProcessWithTokenW}
 * from code inside {@code AdminExecuter.runAsAdmin} (elevated helper). A JVM started as another user via {@code RUN_AS_USER_TASK} is often
 * <strong>not</strong> elevated ({@code TokenElevationTypeLimited} / {@code WindowsUtils.isElevated()==false}) and then lacks privileges for
 * cross-SID token launch (Win32 1314). Use {@link #logCallerTokenDiagnosticsForTokenLaunch(String)} in logs to confirm.
 * <p>
 * UAC elevation (ShellExecuteEx {@code runas}) is available via {@link #runUACElevated(String[], RunAsLaunchOptions)} ({@code waitFor=false} returns
 * {@link org.appwork.utils.processes.ProcessOutput#getProcessInfo()}). {@link org.appwork.utils.os.WindowsUtils#startElevatedProcess} is a thin legacy wrapper.
 * <p>
 * Same-user admin downgrade (unelevated child, same account — not session-owner launch) is {@link #runNonElevated(String[], RunAsLaunchOptions)}:
 * WinSafer NORMALUSER + medium integrity. UAC {@code TokenLinkedToken} is not
 * used — without {@code SeTcbPrivilege} it is only Identification-level / not launchable. Distinct from {@link #runInOwnerSession}.
 */
public final class RunAsHelper {
    private static final int TOKEN_TYPE_PRIMARY             = 1;
    private static final int SECURITY_IMPERSONATION_LEVEL_2 = 2;
    /**
     * Access mask for duplicated primary tokens used for process creation APIs. CreateProcessWithTokenW is sensitive to missing
     * TOKEN_ASSIGN_PRIMARY/TOKEN_IMPERSONATE rights.
     */
    private static final int DUPLICATE_PRIMARY_TOKEN_ACCESS = WinNT.TOKEN_ASSIGN_PRIMARY | WinNT.TOKEN_DUPLICATE | WinNT.TOKEN_QUERY | WinNT.TOKEN_ADJUST_DEFAULT | WinNT.TOKEN_ADJUST_SESSIONID | WinNT.TOKEN_IMPERSONATE;

    private static void debug(String msg) {
        LogV3.info("RunAsHelperDebug: " + msg);
    }

    private RunAsHelper() {
    }

    /**
     * Step 2: resolves the interactive user for the <strong>current process</strong> WTS session (session id, SID, token). Caller must
     * {@link InteractiveSessionOwner#close()} the result.
     */
    public static InteractiveSessionOwner resolveInteractiveOwnerForCurrentProcess() throws Exception {
        ensureWindowsAndJna();
        return InteractiveSessionOwner.openForCurrentProcess();
    }

    /**
     * Step 2: same as {@link #resolveInteractiveOwnerForCurrentProcess()} but for an explicit WTS session id (e.g. LocalSystem
     * targeting a user session).
     */
    public static InteractiveSessionOwner resolveInteractiveOwnerForSession(int sessionId) throws Exception {
        ensureWindowsAndJna();
        return InteractiveSessionOwner.openForSession(sessionId);
    }

    /**
     * Step 1+2 snapshot for the current process/session: process context (session/SID/elevation/system) plus owner context for that
     * session.
     */
    public static RunAsCurrentContext resolveCurrentContext() throws Exception {
        ensureWindowsAndJna();
        int processSession = WindowsUtils.getCurrentProcessSessionId();
        if (processSession < 0) {
            throw new IllegalStateException("Cannot resolve WTS session id for current process (ProcessIdToSessionId failed).");
        }
        String processSid = WindowsUtils.getCurrentUserSID();
        boolean elevated = WindowsUtils.isElevated();
        boolean localSystem = WindowsUtils.isRunningAsLocalSystem();
        // Same rule as InteractiveSessionOwner.openForCurrentProcess: LocalSystem/session 0 -> interactive owner session
        int ownerSession = InteractiveSessionOwner.resolveOwnerSessionIdForCurrentProcess();
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForSession(ownerSession);
        try {
            return new RunAsCurrentContext(processSession, processSid, elevated, localSystem, owner.getSessionId(), owner.getOwnerSid(), owner.getOwnerAccountName());
        } finally {
            owner.close();
        }
    }

    /**
     * Step 1+2 snapshot for an explicit owner session id, keeping current caller process facts and resolving owner facts for
     * {@code ownerSessionId}.
     */
    public static RunAsCurrentContext resolveCurrentContextForOwnerSession(int ownerSessionId) throws Exception {
        ensureWindowsAndJna();
        int processSession = WindowsUtils.getCurrentProcessSessionId();
        if (processSession < 0) {
            throw new IllegalStateException("Cannot resolve WTS session id for current process (ProcessIdToSessionId failed).");
        }
        String processSid = WindowsUtils.getCurrentUserSID();
        boolean elevated = WindowsUtils.isElevated();
        boolean localSystem = WindowsUtils.isRunningAsLocalSystem();
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForSession(ownerSessionId);
        try {
            return new RunAsCurrentContext(processSession, processSid, elevated, localSystem, owner.getSessionId(), owner.getOwnerSid(), owner.getOwnerAccountName());
        } finally {
            owner.close();
        }
    }

    /**
     * Runs {@code cmd} as the owner of the <strong>current process</strong> interactive session. Resolves session/SID/token internally; no
     * session id parameter.
     */
    public static ProcessOutput runInOwnerSession(String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForCurrentProcess();
        LogV3.info("Execute as " + owner + ": " + Arrays.asList(cmd) + "Options: " + options);
        try {
            return executeForResolvedOwner(owner, cmd, options != null ? options : RunAsLaunchOptions.DEFAULT);
        } finally {
            owner.close();
        }
    }

    /**
     * Runs {@code cmd} as the interactive owner of the given WTS {@code sessionId}.
     * <p>
     * Typical caller is LocalSystem (or another principal with {@code SeTcbPrivilege}), because resolving the session user token uses
     * {@code WTSQueryUserToken}. There is no separate LocalSystem check in this method — without sufficient rights the token open fails.
     * Prefer {@link #runInOwnerSession} when the target is the current process session.
     */
    public static ProcessOutput runInSession(int sessionId, String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForSession(sessionId);
        try {
            return executeForResolvedOwner(owner, cmd, options != null ? options : RunAsLaunchOptions.DEFAULT);
        } finally {
            owner.close();
        }
    }

    /**
     * Runs {@code cmd} under the session owner of the <strong>current process</strong> session; {@code expectedSid} must match that owner's
     * SID.
     */
    public static ProcessOutput runAsUser(String expectedSid, String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (expectedSid == null || expectedSid.trim().length() == 0) {
            throw new IllegalArgumentException("expectedSid is required");
        }
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForCurrentProcess();
        try {
            verifyExpectedSidMatchesOwner(owner, expectedSid.trim());
            return executeForResolvedOwner(owner, cmd, options != null ? options : RunAsLaunchOptions.DEFAULT);
        } finally {
            owner.close();
        }
    }

    /**
     * Runs {@code cmd} as the owner of {@code sessionId}; {@code expectedSid} must match that session owner's SID. For callers not in
     * the target session (e.g. LocalSystem).
     */
    public static ProcessOutput runAsUserInSession(int sessionId, String expectedSid, String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (expectedSid == null || expectedSid.trim().length() == 0) {
            throw new IllegalArgumentException("expectedSid is required");
        }
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        InteractiveSessionOwner owner = InteractiveSessionOwner.openForSession(sessionId);
        try {
            verifyExpectedSidMatchesOwner(owner, expectedSid.trim());
            return executeForResolvedOwner(owner, cmd, options != null ? options : RunAsLaunchOptions.DEFAULT);
        } finally {
            owner.close();
        }
    }

    /**
     * Starts {@code cmd} with administrator rights via ShellExecuteEx {@code runas} (UAC consent dialog). When the current process is already
     * elevated, runs in the interactive owner session without a second UAC prompt (same strategy as other {@link RunAsHelper} entry points).
     * <p>
     * Stdout/stderr are only captured when {@link RunAsLaunchOptions#isWaitFor()} is {@code true} and the process was started without UAC
     * (already elevated); the ShellExecuteEx path does not attach pipes. With {@code waitFor=false}, {@link org.appwork.utils.processes.ProcessOutput#getProcessInfo()}
     * holds a {@link JNAProcessInfo} with an open handle until {@link JNAProcessInfo#close()}.
     *
     * @param cmd
     *            executable and arguments (argv[0] = binary)
     * @param options
     *            working directory, wait, showWindow, noErrorUI, etc.
     */
    public static ProcessOutput runUACElevated(String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        final RunAsLaunchOptions opts = options != null ? options : RunAsLaunchOptions.DEFAULT;
        if (WindowsUtils.isElevated()) {
            debug("runUACElevated: caller already elevated -> owner-session launch (no UAC)");
            return runInOwnerSession(cmd, opts);
        }
        debug("runUACElevated: ShellExecuteEx runas showWindow=" + opts.isShowWindow() + " noErrorUI=" + opts.isNoErrorUI() + " waitFor=" + opts.isWaitFor());
        final JNAProcessInfo proc = RunAsUacLauncher.shellExecuteRunAs(cmd, opts);
        try {
            if (!opts.isWaitFor()) {
                debug("runUACElevated: ShellExecuteEx started pid=" + proc.getPid() + " (waitFor=false)");
                return new ProcessOutput(-1, new ByteArrayOutputStream(), new ByteArrayOutputStream(), getConsoleCodepageSafe(), null, null, proc);
            }
            return waitForJnaProcessAndBuildOutput(proc);
        } finally {
            if (opts.isWaitFor()) {
                proc.close();
            }
        }
    }

    /**
     * @see #runUACElevated(String[], RunAsLaunchOptions)
     */
    public static ProcessOutput runUACElevated(String[] cmd) throws Exception {
        return runUACElevated(cmd, RunAsLaunchOptions.DEFAULT);
    }

    /**
     * Runs {@code cmd} as the <strong>same account</strong> without elevation (admin downgrade). Does not switch to the interactive
     * session owner — use {@link #runInOwnerSession} for that.
     * <p>
     * If the caller is not elevated, uses {@link ProcessBuilder}. If elevated, builds an unelevated same-user token via WinSafer
     * {@code NORMALUSER} + medium integrity, then launches with {@code CreateProcessWithTokenW}. LocalSystem is rejected (use
     * {@link #runInSession}).
     * <p>
     * UAC {@code TokenLinkedToken} is intentionally not used: without {@code SeTcbPrivilege} Windows returns only an Identification-level
     * handle that cannot launch processes.
     */
    public static ProcessOutput runNonElevated(String[] cmd, RunAsLaunchOptions options) throws Exception {
        ensureWindowsAndJna();
        if (cmd == null || cmd.length == 0) {
            throw new IllegalArgumentException("cmd cannot be null or empty");
        }
        final RunAsLaunchOptions opts = options != null ? options : RunAsLaunchOptions.DEFAULT;
        if (WindowsUtils.isRunningAsLocalSystem()) {
            throw new IllegalStateException("runNonElevated is not supported under LocalSystem (same-user downgrade would remain SYSTEM). Use runInSession for the interactive session owner.");
        }
        if (!WindowsUtils.isElevated()) {
            debug("runNonElevated: caller not elevated -> ProcessBuilder");
            return runViaProcessBuilder(cmd, opts);
        }
        logCallerTokenDiagnosticsForTokenLaunch("runNonElevated_SAFER");
        WinNT.HANDLE primary = null;
        try {
            primary = createPrimaryTokenFromSafer();
            debug("runNonElevated: using WinSafer NORMALUSER + medium integrity");
            logTokenDiagnosticsForLaunch(primary, "runNonElevated_beforeLaunch");
            // Elevated admin typically lacks SeAssignPrimaryTokenPrivilege — prefer CreateProcessWithTokenW.
            return runNonElevatedTokenLaunch(primary, cmd, opts);
        } finally {
            closeHandleQuietly(primary);
        }
    }

    /** Also write to System.err so AdminExecuter task-failure stderr includes the line. */
    private static void debugErr(final String msg) {
        debug(msg);
        try {
            System.err.println("RunAsHelperDebug: " + msg);
        } catch (Throwable ignore) {
        }
    }

    /**
     * Launch for {@link #runNonElevated}: CreateProcessWithTokenW only (logonFlags 0 then {@code LOGON_WITH_PROFILE}). Does not fall back to
     * CreateProcessAsUserW — elevated admins usually lack AssignPrimary and the AsUser error (often 1314/1346) obscures the real WithToken failure.
     */
    private static ProcessOutput runNonElevatedTokenLaunch(final WinNT.HANDLE primary, final String[] cmd, final RunAsLaunchOptions opts) throws Exception {
        logTokenDiagnosticsForLaunch(primary, "runNonElevatedTokenLaunch");
        try {
            debugErr("runNonElevatedTokenLaunch: CreateProcessWithTokenW logonFlags=0");
            return RunAsProcessLauncher.runWithPrimaryTokenUsingCreateProcessWithToken(primary, cmd, opts, 0);
        } catch (Win32Exception first) {
            debugErr("runNonElevatedTokenLaunch: WithToken(flags=0) FAIL gle=" + first.getErrorCode() + " msg=" + first.getMessage() + " -> retry LOGON_WITH_PROFILE");
            try {
                return RunAsProcessLauncher.runWithPrimaryTokenUsingCreateProcessWithToken(primary, cmd, opts, Advapi32CreateProcessWithTokenLib.LOGON_WITH_PROFILE);
            } catch (Win32Exception second) {
                debugErr("runNonElevatedTokenLaunch: WithToken(LOGON_WITH_PROFILE) FAIL gle=" + second.getErrorCode() + " msg=" + second.getMessage() + " (flags=0 was gle=" + first.getErrorCode() + ")");
                throw new Win32Exception(second.getErrorCode());
            }
        }
    }

    /**
     * WinSafer NORMALUSER level, then set medium integrity. Returns a primary token; caller must close.
     */
    private static WinNT.HANDLE createPrimaryTokenFromSafer() throws Exception {
        WinNT.HANDLE levelHandle = null;
        WinNT.HANDLE hToken = null;
        Pointer sidToFree = null;
        try {
            final HANDLEByReference pLevel = new HANDLEByReference();
            RunAsWin32ApiTrace.in("RunAsHelper", "SaferCreateLevel", "scope=USER level=NORMALUSER");
            final boolean levelOk = Advapi32SaferLib.INSTANCE.SaferCreateLevel(Advapi32SaferLib.SAFER_SCOPEID_USER, Advapi32SaferLib.SAFER_LEVELID_NORMALUSER, Advapi32SaferLib.SAFER_LEVEL_OPEN, pLevel, null);
            final int gleLevel = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunAsHelper", "SaferCreateLevel", levelOk, gleLevel);
            if (!levelOk) {
                throw new Win32Exception(gleLevel);
            }
            levelHandle = pLevel.getValue();
            final HANDLEByReference pToken = new HANDLEByReference();
            RunAsWin32ApiTrace.in("RunAsHelper", "SaferComputeTokenFromLevel", "inAccessToken=null(currentProcess)");
            final boolean tokenOk = Advapi32SaferLib.INSTANCE.SaferComputeTokenFromLevel(levelHandle, null, pToken, 0, null);
            final int gleToken = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunAsHelper", "SaferComputeTokenFromLevel", tokenOk, gleToken);
            if (!tokenOk) {
                throw new Win32Exception(gleToken);
            }
            hToken = pToken.getValue();
            final PSIDByReference pSid = new PSIDByReference();
            if (!Advapi32.INSTANCE.ConvertStringSidToSid(Advapi32SaferLib.INTEGRITY_SID_MEDIUM, pSid)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            sidToFree = pSid.getValue().getPointer();
            final int tmlStructSize = new Advapi32SaferLib.TOKEN_MANDATORY_LABEL().size();
            final Memory tmlBuf = new Memory(tmlStructSize);
            tmlBuf.setPointer(0, pSid.getValue().getPointer());
            tmlBuf.setInt(Native.POINTER_SIZE, Advapi32SaferLib.SE_GROUP_INTEGRITY);
            final int tmlSize = tmlStructSize + Advapi32.INSTANCE.GetLengthSid(pSid.getValue());
            RunAsWin32ApiTrace.in("RunAsHelper", "SetTokenInformation", "TokenIntegrityLevel medium size=" + tmlSize);
            final boolean setOk = Advapi32SaferLib.INSTANCE.SetTokenInformation(hToken, WinNT.TOKEN_INFORMATION_CLASS.TokenIntegrityLevel, tmlBuf, tmlSize);
            final int gleSet = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunAsHelper", "SetTokenInformation(TokenIntegrityLevel)", setOk, gleSet);
            if (!setOk) {
                throw new Win32Exception(gleSet);
            }
            final WinNT.HANDLE result = hToken;
            hToken = null; // ownership transferred to caller
            return result;
        } finally {
            if (levelHandle != null) {
                Advapi32SaferLib.INSTANCE.SaferCloseLevel(levelHandle);
            }
            closeHandleQuietly(hToken);
            if (sidToFree != null) {
                Kernel32.INSTANCE.LocalFree(sidToFree);
            }
        }
    }

    /**
     * Resolves a Windows known-folder path for the given user token via {@code SHGetKnownFolderPath}.
     *
     * @param token
     *            user token handle (may be null for current user)
     * @param knownFolderId
     *            KNOWNFOLDERID GUID
     * @param flags
     *            SHGetKnownFolderPath flags (see {@link ShlObj.KNOWN_FOLDER_FLAG})
     * @return resolved absolute path, without trailing separator
     */
    public static String getKnownFolderPath(final WinNT.HANDLE token, final GUID knownFolderId, final int flags) {
        ensureWindowsAndJna();
        if (knownFolderId == null) {
            throw new IllegalArgumentException("knownFolderId cannot be null");
        }
        final PointerByReference outPath = new PointerByReference();
        final WinNT.HRESULT hr = Shell32.INSTANCE.SHGetKnownFolderPath(knownFolderId, flags, token, outPath);
        if (!W32Errors.SUCCEEDED(hr.intValue())) {
            throw new Win32Exception(hr);
        }
        try {
            return outPath.getValue().getWideString(0);
        } finally {
            if (outPath.getValue() != null) {
                Ole32.INSTANCE.CoTaskMemFree(outPath.getValue());
            }
        }
    }

    /**
     * Package entry for {@link InteractiveSessionOwner} (duplicate guard).
     */
    static void ensureWindowsAndJna() {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("RunAsHelper is only supported on Windows");
        }
        if (!JNAHelper.isJNAAvailable()) {
            throw new UnsupportedOperationException("RunAsHelper requires JNA");
        }
    }

    private static void verifyExpectedSidMatchesOwner(InteractiveSessionOwner owner, String expectedSid) {
        String actual = owner.getOwnerSid();
        if (actual == null || expectedSid == null || !actual.equalsIgnoreCase(expectedSid)) {
            throw new IllegalStateException("expectedSid does not match resolved session owner SID (expected " + expectedSid + ", owner " + actual + ").");
        }
    }

    private enum ExecutionKind {
        /** Normal child in this JVM identity (no token launch API). */
        PROCESS_BUILDER,
        /** Duplicate session token and launch via token-based API. */
        TOKEN_LAUNCH
    }

    /**
     * Step 1+3: choose execution strategy from caller + options, then run.
     */
    private static ProcessOutput executeForResolvedOwner(InteractiveSessionOwner owner, String[] cmd, RunAsLaunchOptions opts) throws Exception {
        ExecutionKind kind = planExecution(owner, opts);
        debug("executeForResolvedOwner: kind=" + kind + ", waitFor=" + opts.isWaitFor() + ", ownerSession=" + owner.getSessionId() + ", ownerSid=" + owner.getOwnerSid() + ", currentSession=" + WindowsUtils.getCurrentProcessSessionId() + ", currentSid=" + WindowsUtils.getCurrentUserSID() + ", elevated=" + WindowsUtils.isElevated() + ", localSystem=" + WindowsUtils.isRunningAsLocalSystem());
        if (kind == ExecutionKind.PROCESS_BUILDER) {
            return runViaProcessBuilder(cmd, opts);
        }
        logCallerTokenDiagnosticsForTokenLaunch("executeForResolvedOwner_TOKEN_LAUNCH");
        WinNT.HANDLE primary = null;
        try {
            WinNT.HANDLE sessionToken = owner.getUserTokenHandle();
            if (sessionToken == null) {
                throw new IllegalStateException("Session user token is null (already closed?).");
            }
            primary = duplicateToPrimaryToken(sessionToken);
            return runTokenLaunchWithBestApi(primary, cmd, opts);
        } finally {
            closeHandleQuietly(primary);
        }
    }

    /**
     * Step 1: decide whether a plain {@link java.lang.ProcessBuilder} is sufficient or token-based launch is required.
     */
    private static ExecutionKind planExecution(InteractiveSessionOwner owner, RunAsLaunchOptions opts) throws Exception {
        if (!opts.isWaitFor()) {
            return ExecutionKind.TOKEN_LAUNCH;
        }
        if (WindowsUtils.isElevated()) {
            return ExecutionKind.TOKEN_LAUNCH;
        }
        int curSession = WindowsUtils.getCurrentProcessSessionId();
        if (curSession < 0 || curSession != owner.getSessionId()) {
            return ExecutionKind.TOKEN_LAUNCH;
        }
        String curSid = WindowsUtils.getCurrentUserSID();
        String ownerSid = owner.getOwnerSid();
        if (curSid != null && ownerSid != null && curSid.equalsIgnoreCase(ownerSid)) {
            return ExecutionKind.PROCESS_BUILDER;
        }
        return ExecutionKind.TOKEN_LAUNCH;
    }

    private static ProcessOutput runViaProcessBuilder(String[] cmd, RunAsLaunchOptions opts) throws IOException, InterruptedException {
        java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(cmd);
        if (opts.getWorkingDir() != null) {
            pb.directory(opts.getWorkingDir());
        }
        return ProcessBuilderFactory.runCommand(pb);
    }

    /**
     * Prefers CreateProcessWithTokenW for non-LocalSystem callers (closer to working POC behavior). Falls back to CreateProcessAsUserW when
     * WithToken fails. LocalSystem keeps CreateProcessAsUserW first because it usually has the required privileges and is the established
     * path.
     */
    private static ProcessOutput runTokenLaunchWithBestApi(WinNT.HANDLE primary, String[] cmd, RunAsLaunchOptions opts) throws Exception {
        logTokenDiagnosticsForLaunch(primary, "runTokenLaunchWithBestApi");
        final boolean localSystem = WindowsUtils.isRunningAsLocalSystem();
        if (!localSystem) {
            try {
                debug("tokenLaunch: first try CreateProcessWithTokenW (LOGON_WITH_PROFILE)");
                return RunAsProcessLauncher.runWithPrimaryTokenUsingCreateProcessWithToken(primary, cmd, opts);
            } catch (Win32Exception withTokenError) {
                debug("tokenLaunch: CreateProcessWithTokenW FAILED err=" + withTokenError.getErrorCode() + " msg=" + withTokenError.getMessage() + " -> retry CreateProcessAsUserW");
                try {
                    return RunAsProcessLauncher.runWithPrimaryToken(primary, cmd, opts);
                } catch (Win32Exception asUserError) {
                    debug("tokenLaunch: CreateProcessAsUserW ALSO FAILED err=" + asUserError.getErrorCode() + " msg=" + asUserError.getMessage() + " (WithToken was err=" + withTokenError.getErrorCode() + " msg=" + withTokenError.getMessage() + ")");
                    throw asUserError;
                }
            }
        } else {
            try {
                debug("tokenLaunch: LocalSystem first try CreateProcessAsUserW");
                return RunAsProcessLauncher.runWithPrimaryToken(primary, cmd, opts);
            } catch (Win32Exception asUserError) {
                debug("tokenLaunch: LocalSystem CreateProcessAsUserW failed err=" + asUserError.getErrorCode() + ", msg=" + asUserError.getMessage() + " -> retry CreateProcessWithTokenW");
                return RunAsProcessLauncher.runWithPrimaryTokenUsingCreateProcessWithToken(primary, cmd, opts);
            }
        }
    }

    /**
     * Debug: dump TokenType / Elevation / SessionId for a launch token (search logs for {@code RunAsTokenDiag}).
     */
    static void logTokenDiagnosticsForLaunch(final WinNT.HANDLE token, final String tag) {
        if (token == null) {
            debug("RunAsTokenDiag[" + tag + "]: token=null");
            return;
        }
        try {
            final int tokenType = readTokenInformationInt32(token, WinNT.TOKEN_INFORMATION_CLASS.TokenType);
            final int elevType = readTokenInformationInt32(token, WinNT.TOKEN_INFORMATION_CLASS.TokenElevationType);
            final int isElevated = readTokenInformationInt32(token, WinNT.TOKEN_INFORMATION_CLASS.TokenElevation);
            final int sessionId = readTokenInformationInt32(token, WinNT.TOKEN_INFORMATION_CLASS.TokenSessionId);
            final int impLevel = readTokenInformationInt32(token, WinNT.TOKEN_INFORMATION_CLASS.TokenImpersonationLevel);
            String sid = "?";
            try {
                final com.sun.jna.platform.win32.Advapi32Util.Account acc = Advapi32Util.getTokenAccount(token);
                if (acc != null && acc.sidString != null) {
                    sid = acc.sidString;
                }
            } catch (Throwable t) {
                sid = "err:" + t.getMessage();
            }
            debug("RunAsTokenDiag[" + tag + "]: handle=" + RunAsWin32ApiTrace.h(token) + " TokenType=" + tokenType + "(1=Primary,2=Impersonation)" + " ElevationType=" + elevType + "(" + tokenElevationTypeLabel(elevType) + ")" + " TokenIsElevated=" + isElevated + " TokenSessionId=" + sessionId + " TokenImpersonationLevel=" + impLevel + "(0=Anon,1=Ident,2=Impersonation,3=Delegation;-1=n/a)" + " sid=" + sid + " callerElevated=" + WindowsUtils.isElevated() + " callerSession=" + WindowsUtils.getCurrentProcessSessionId());
            try {
                System.err.println("RunAsHelperDebug: RunAsTokenDiag[" + tag + "]: TokenType=" + tokenType + " ElevationType=" + elevType + " TokenIsElevated=" + isElevated + " TokenSessionId=" + sessionId + " ImpLevel=" + impLevel + " sid=" + sid);
            } catch (Throwable ignore) {
            }
        } catch (Throwable t) {
            debug("RunAsTokenDiag[" + tag + "]: failed " + t);
        }
    }

    private static WinNT.HANDLE duplicateToPrimaryToken(WinNT.HANDLE source) {
        RunAsWin32ApiTrace.in("RunAsHelper", "DuplicateTokenEx", "source=" + RunAsWin32ApiTrace.h(source) + " desiredAccess=0x" + Integer.toHexString(DUPLICATE_PRIMARY_TOKEN_ACCESS) + " impersonationLevel=" + SECURITY_IMPERSONATION_LEVEL_2 + " tokenType=" + TOKEN_TYPE_PRIMARY);
        HANDLEByReference pPrimary = new HANDLEByReference();
        final boolean ok = Advapi32DuplicateTokenExLib.INSTANCE.DuplicateTokenEx(source, DUPLICATE_PRIMARY_TOKEN_ACCESS, null, SECURITY_IMPERSONATION_LEVEL_2, TOKEN_TYPE_PRIMARY, pPrimary);
        final int gle = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunAsHelper", "DuplicateTokenEx", ok, gle);
        if (!ok) {
            throw new Win32Exception(gle);
        }
        return pPrimary.getValue();
    }

    private static void closeHandleQuietly(WinNT.HANDLE h) {
        if (h != null) {
            try {
                RunAsWin32ApiTrace.in("RunAsHelper", "CloseHandle", RunAsWin32ApiTrace.h(h));
                final boolean c = Kernel32.INSTANCE.CloseHandle(h);
                RunAsWin32ApiTrace.out("RunAsHelper", "CloseHandle", c, Kernel32.INSTANCE.GetLastError());
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }

    private static ProcessOutput waitForJnaProcessAndBuildOutput(JNAProcessInfo proc) throws InterruptedException {
        final WinNT.HANDLE hProcess = proc.getHandle();
        RunAsWin32ApiTrace.in("RunAsHelper", "WaitForSingleObject", "hProcess=" + RunAsWin32ApiTrace.h(hProcess) + " timeout=INFINITE");
        final int waitRc = Kernel32.INSTANCE.WaitForSingleObject(hProcess, Kernel32.INFINITE);
        final int gleWait = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunAsHelper", "WaitForSingleObject", true, gleWait);
        if (RunAsWin32ApiTrace.isEnabled()) {
            LogV3.info(RunAsWin32ApiTrace.PREFIX + " RunAsHelper INFO WaitForSingleObject returnCode=" + waitRc + " gle=" + gleWait);
        }
        final IntByReference exitCodeRef = new IntByReference();
        RunAsWin32ApiTrace.in("RunAsHelper", "GetExitCodeProcess", "hProcess=" + RunAsWin32ApiTrace.h(hProcess));
        final boolean gec = Kernel32.INSTANCE.GetExitCodeProcess(hProcess, exitCodeRef);
        RunAsWin32ApiTrace.out("RunAsHelper", "GetExitCodeProcess", gec, Kernel32.INSTANCE.GetLastError());
        final int exitCode = gec ? exitCodeRef.getValue() : -1;
        return new ProcessOutput(exitCode, new ByteArrayOutputStream(), new ByteArrayOutputStream(), getConsoleCodepageSafe());
    }

    private static String getConsoleCodepageSafe() {
        try {
            return ProcessBuilderFactory.getConsoleCodepage();
        } catch (Throwable t) {
            return "UTF-8";
        }
    }

    /**
     * Removable diagnostics: why {@code CreateProcessWithTokenW} may return 1314 ({@code ERROR_PRIVILEGE_NOT_HELD}) — compare caller to
     * {@code org.appwork.jna.windows.experimental.ide.DesktopSessionCheck} (elevated helper).
     */
    public static void logCallerTokenDiagnosticsForTokenLaunch(String tag) {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            return;
        }
        WinNT.HANDLE hToken = null;
        try {
            HANDLEByReference ph = new HANDLEByReference();
            RunAsWin32ApiTrace.in("RunAsHelper", "OpenProcessToken", "process=GetCurrentProcess desiredAccess=TOKEN_QUERY tag=" + tag);
            final boolean opened = Advapi32.INSTANCE.OpenProcessToken(Kernel32.INSTANCE.GetCurrentProcess(), WinNT.TOKEN_QUERY, ph);
            int gle = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("RunAsHelper", "OpenProcessToken", opened, gle);
            if (!opened) {
                LogV3.info("RunAsCallerDiag[" + tag + "]: OpenProcessToken failed lastError=" + gle);
                return;
            }
            hToken = ph.getValue();
            final int elType = readTokenInformationInt32(hToken, WinNT.TOKEN_INFORMATION_CLASS.TokenElevationType);
            final int tokenIsElevated = readTokenInformationInt32(hToken, WinNT.TOKEN_INFORMATION_CLASS.TokenElevation);
            LogV3.info("RunAsCallerDiag[" + tag + "]: user=" + Advapi32Util.getUserName() + " session=" + WindowsUtils.getCurrentProcessSessionId() + " sid=" + WindowsUtils.getCurrentUserSID() + " WindowsUtils.isElevated=" + WindowsUtils.isElevated() + " TokenElevationType=" + elType + "(" + tokenElevationTypeLabel(elType) + ")" + " TokenIsElevated=" + tokenIsElevated + " hint=1314_if_not_elevated_or_no_assignprimary");
        } catch (Throwable t) {
            LogV3.info("RunAsCallerDiag[" + tag + "]: " + t);
        } finally {
            closeHandleQuietly(hToken);
        }
    }

    private static String tokenElevationTypeLabel(final int v) {
        if (v == 1) {
            return "Default";
        }
        if (v == 2) {
            return "Full";
        }
        if (v == 3) {
            return "Limited";
        }
        if (v < 0) {
            return "query_failed";
        }
        return "unknown";
    }

    private static int readTokenInformationInt32(final WinNT.HANDLE token, final int infoClass) {
        final IntByReference retLen = new IntByReference();
        RunAsWin32ApiTrace.in("RunAsHelper", "GetTokenInformation", "token=" + RunAsWin32ApiTrace.h(token) + " class=" + infoClass + " phase=size_probe");
        final boolean probeOk = Advapi32.INSTANCE.GetTokenInformation(token, infoClass, null, 0, retLen);
        int gle = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunAsHelper", "GetTokenInformation(size_probe)", probeOk, gle);
        if (RunAsWin32ApiTrace.isEnabled() && !probeOk && gle == W32Errors.ERROR_INSUFFICIENT_BUFFER) {
            LogV3.info(RunAsWin32ApiTrace.PREFIX + " RunAsHelper INFO GetTokenInformation size_probe: return=false gle=122 is expected for buffer sizing");
        }
        if (!probeOk) {
            if (gle != W32Errors.ERROR_INSUFFICIENT_BUFFER) {
                return -1;
            }
        }
        final int size = Math.max(4, retLen.getValue());
        final DwordTokenBuffer buf = new DwordTokenBuffer();
        buf.clear();
        retLen.setValue(0);
        RunAsWin32ApiTrace.in("RunAsHelper", "GetTokenInformation", "token=" + RunAsWin32ApiTrace.h(token) + " class=" + infoClass + " length=" + size);
        final boolean readOk = Advapi32.INSTANCE.GetTokenInformation(token, infoClass, buf, size, retLen);
        gle = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunAsHelper", "GetTokenInformation(read)", readOk, gle);
        if (!readOk) {
            return -2;
        }
        return buf.value;
    }

    /**
     * DWORD-sized token info buffer for {@link Advapi32#GetTokenInformation}. Must be {@code public} so JNA {@link Structure} can reflect
     * the field from {@code com.sun.jna.Structure}.
     */
    public static final class DwordTokenBuffer extends Structure {
        public int value;

        public DwordTokenBuffer() {
            super();
        }

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList("value");
        }
    }
}