/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.IOException;
import java.io.Serializable;
import java.util.List;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Time;
import org.appwork.utils.Timeout;
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.InteractiveSessionOwner;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.KnownFolders;
import com.sun.jna.platform.win32.ShlObj;
import com.sun.jna.platform.win32.Win32Exception;

/**
 * Tests for {@link RunAsHelper} (WTS session + {@code CreateProcessAsUser}). Windows + JNA only.
 * <p>
 * What runs here (in order):
 * <ol>
 * <li>{@code runInOwnerSession} — child must be same WTS session + same user SID as this JVM (PowerShell probe + {@code whoami}).</li>
 * <li>{@code runAsUser} — wrong SID is rejected; matching SID runs {@code whoami}.</li>
 * <li>Cross-user — new local user: own probe vs {@code runInOwnerSession} probe; session probe must match this JVM baseline.</li>
 * <li>Cross-user admin downgrade — {@code runAsUser} starts the task JVM as a filtered local admin; the owner-session probe runs via nested
 * {@link AdminExecuter#runAsAdmin}, which uses {@link org.appwork.utils.os.WindowsUtils#startElevatedProcess} for that user (not the shared
 * helper), then {@link RunAsHelper#runInOwnerSession} inside the elevated child JVM.</li>
 * <li>Elevated same-user — from an elevated process (same account), {@code runInOwnerSession} must return to the interactive non-elevated
 * owner token (same session + same SID, elevation lowered).</li>
 * <li>{@link RunAsHelper#runUACElevated} — ShellExecuteEx {@code runas} (UAC), {@link ProcessOutput#getProcessInfo()}, optional
 * {@code waitFor}; same scenarios as {@link org.appwork.utils.os.tests.TestWindowsUtils#testProcessManagement} for
 * {@link WindowsUtils#startElevatedProcess}.</li>
 * </ol>
 * Uses {@link AdminExecuter#runAsAdmin} and {@link AdminExecuter#runAsUser}.
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.utils.os.windows.execute.InteractiveSessionOwner", "org.appwork.utils.os.JNAProcessInfo", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsHelper extends AWTest {
    /** Win32 {@code ERROR_CANCELLED} when the user dismisses the UAC dialog. */
    private static final int ERROR_CANCELLED_UAC = 1223;

    /** Verifies the test host can run elevated before UAC launch tests. */
    private static final class ElevatedCheckTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public java.io.Serializable run() throws Exception {
            if (!WindowsUtils.isElevated()) {
                throw new Exception("Expected elevated process for UAC pre-check");
            }
            return null;
        }
    }

    /** From an elevated JVM, {@link RunAsHelper#runUACElevated} must use owner-session launch (no second UAC). */
    private static final class RunUACElevatedFromElevatedTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public java.io.Serializable run() throws Exception {
            if (!WindowsUtils.isElevated()) {
                throw new Exception("task JVM must be elevated");
            }
            final ProcessOutput out = RunAsHelper.runUACElevated(new String[] { "cmd.exe", "/c", "exit", "42" }, RunAsLaunchOptions.builder().waitFor(true).build());
            if (out.getProcessInfo() != null) {
                throw new Exception("elevated caller: expected no ProcessInfo on ProcessOutput, got " + out.getProcessInfo());
            }
            if (out.getExitCode() != 42) {
                throw new Exception("elevated caller: expected exit 42, got " + out.getExitCode() + " stderr=" + out.getErrOutString());
            }
            return null;
        }
    }

    /**
     * First line (machine): {@code WTS_SESSION|USER_SID|ELEV} where {@code ELEV} is {@code 1} if the process token includes high integrity
     * ({@code S-1-16-12288}), else {@code 0}. Following lines are human-readable ({@code session=…}, {@code sid=…}, {@code elev=…}) for
     * logs; {@link #parseProbe(String)} uses the first stdout line only (trimmed).
     */
    private static final String PS_PROBE_SESSION_SID_ELEV = "$s=[int]([Diagnostics.Process]::GetCurrentProcess().SessionId);" + "$id=[System.Security.Principal.WindowsIdentity]::GetCurrent().User.Value;" + "$pr=New-Object System.Security.Principal.WindowsPrincipal([System.Security.Principal.WindowsIdentity]::GetCurrent());" + "$hi=$pr.IsInRole([System.Security.Principal.SecurityIdentifier]::new('S-1-16-12288'));" + "$e=if($hi){'1'}else{'0'};" + "Write-Output ($s.ToString()+'|'+$id+'|'+$e);" + "Write-Output ('session='+$s);Write-Output ('sid='+$id);Write-Output ('elev='+$e)";

    /**
     * Parsed first machine line of {@link #PS_PROBE_SESSION_SID_ELEV} output: {@code session|sid|elev}. {@link #equals(Object)} compares
     * {@link #machineLine}.
     */
    private static final class ProbeResult implements Serializable {
        private static final long serialVersionUID = 1L;
        public final int          session;
        public final String       sid;
        public final String       elev;
        /** Canonical line (SID lowercased) for stable {@link #equals(Object)}; includes elev as third segment. */
        public final String       machineLine;

        ProbeResult(final int session, final String sid, final String elev, final String machineLine) {
            this.session = session;
            this.sid = sid != null ? sid : "";
            this.elev = elev != null ? elev : "";
            this.machineLine = machineLine != null ? machineLine : "";
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof ProbeResult)) {
                return false;
            }
            final ProbeResult other = (ProbeResult) o;
            return machineLine.equals(other.machineLine);
        }

        @Override
        public int hashCode() {
            return machineLine.hashCode();
        }

        @Override
        public String toString() {
            return machineLine;
        }
    }

    /**
     * Own-process probe (same rules as {@link #getCurrentContext()}) plus optional session-owner fields filled via
     * {@link RunAsHelper#runInOwnerSession} or (in cross-admin downgrade test) a nested {@link AdminExecuter#runAsAdmin} task that runs the
     * same probe from the elevated helper JVM.
     */
    public static final class OwnerSessionWtsProbeOutcome implements Serializable {
        private static final long serialVersionUID = 1L;
        /** Parsed probe in the task JVM ({@link ProcessBuilderFactory}, same as {@link #getCurrentContext()}). */
        public ProbeResult        ownContext;
        /** Independent Java-side elevation check from the task JVM ({@link WindowsUtils#isElevated()}). */
        public boolean            ownWindowsUtilsElevated;
        public int                ownExitCode;
        public String             ownStdout;
        public String             ownStderr;
        /** Parsed probe via {@link RunAsHelper#runInOwnerSession}. */
        public ProbeResult        sessionOwnerContext;
        public int                sessionExitCode;
        public String             sessionStdout;
        public String             sessionStderr;
        /** Roaming AppData resolved for the owner-session token via RunAsHelper#getKnownFolderPath(token,...). */
        public String             sessionOwnerRoamingAppData;
        /** Owner SID resolved in a runAsUser task (for explicit owner-check test). */
        public String             ownerSidInTask;

        public OwnerSessionWtsProbeOutcome() {
        }
    }

    private static final TypeRef<OwnerSessionWtsProbeOutcome> TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME = new TypeRef<OwnerSessionWtsProbeOutcome>() {
    };

    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.AWTest#isMaintenance()
     */
    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsHelper: Windows only, skipped.");
            return;
        }
        if (!JNAHelper.isJNAAvailable()) {
            logInfoAnyway("TestRunAsHelper: JNA not available, skipped.");
            return;
        }
        LogV3.info("Current Context: " + getCurrentContext());
        testRunAsUserOwnerAppDataRoundtrip();
        testRunInOwnerSessionRunsAsCurrentUserInCurrentSession();
        testRunAsUserWrongSidThrows();
        testRunAsUserMatchingSidWhoami();
        testRunAsAnotherUser();
        testDowngradeToOwnerSessionFromAnotherAdminUser();
        testRunInOwnerSessionFromElevatedSameUser();
        testRunUACElevatedRejectsEmptyCommand();
        testRunUACElevatedFromElevatedJvm();
        testRunUACElevatedProcessManagement();
        testRunUACElevatedWaitForExit();
        testWindowsUtilsStartElevatedProcessWrapper();
    }

    /** Own probe: exit 0, empty stderr, parsed first line. */
    private void requireOwnOutcomeOk(final String label, final OwnerSessionWtsProbeOutcome o) throws Exception {
        assertTrue(o != null, label + ": outcome is null");
        assertTrue(o.ownExitCode == 0, label + ": own exit=" + o.ownExitCode + ", stderr=" + o.ownStderr);
        assertTrue(stderrBlank(o.ownStderr), label + ": own stderr not empty: " + o.ownStderr);
        assertTrue(o.ownContext != null, label + ": own context null (stdout=" + o.ownStdout + ")");
    }

    /** Session-owner probe via {@link RunAsHelper#runInOwnerSession}: must succeed with empty stderr. */
    private void requireSessionOutcomeOk(final String label, final OwnerSessionWtsProbeOutcome o) throws Exception {
        assertTrue(o.sessionOwnerContext != null, label + ": session owner context null; exit=" + o.sessionExitCode + ", stderr=" + o.sessionStderr + ", stdout=" + o.sessionStdout);
        assertTrue(o.sessionExitCode == 0, label + ": session exit=" + o.sessionExitCode + ", stderr=" + o.sessionStderr);
        assertTrue(stderrBlank(o.sessionStderr), label + ": session stderr not empty: " + o.sessionStderr);
    }

    /**
     * Local admin user (in Administrators) runs in the interactive session; the owner-session probe is executed via nested
     * {@link AdminExecuter#runAsAdmin} (see {@link CrossUserDowngradeToOwnerTask}), which elevates the <strong>same</strong> test user with
     * {@link org.appwork.utils.os.WindowsUtils#startElevatedProcess} instead of the shared helper. Assertions still compare the probe to
     * this JVM's interactive baseline ({@code startContext}).
     */
    private void testDowngradeToOwnerSessionFromAnotherAdminUser() throws Exception {
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("TestRunAsHelper: skip AdminTestUser downgrade test (no session id).");
            return;
        }
        final String userName = "AdminTestUser";
        final String password = Crypto.generateRandomString(10, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(userName, password, true), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final ProbeResult startContext = getCurrentContext();
            final String expectedTestUserSid = resolveLocalUserSid(userName);
            final OwnerSessionWtsProbeOutcome outcome = AdminExecuter.runAsUser(null, userName, password, new CrossUserDowngradeToOwnerTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
            requireOwnOutcomeOk("downgrade", outcome);
            assertFalse(outcome.ownWindowsUtilsElevated, "downgrade: runAsUser child should not be elevated (filtered admin token)");
            assertEquals(interactiveSessionId, outcome.ownContext.session, "downgrade own context session should match interactive session");
            assertEquals(expectedTestUserSid != null ? expectedTestUserSid.toLowerCase() : null, outcome.ownContext.sid != null ? outcome.ownContext.sid.toLowerCase() : null, "downgrade own context SID should match created test user SID");
            assertTrue(!startContext.sid.equalsIgnoreCase(outcome.ownContext.sid), "downgrade own context SID should differ from start context SID");
            logInfoAnyway("TestRunAsHelper: AdminTestUser own context elev=" + outcome.ownContext.elev + " session=" + outcome.ownContext.session + " sid=" + outcome.ownContext.sid);
            requireSessionOutcomeOk("downgrade", outcome);
            assertEquals(startContext, outcome.sessionOwnerContext, "downgrade owner-session context should match start context machine line (session|sid|elev)");
            assertEquals(startContext.session, outcome.sessionOwnerContext.session, "downgrade owner-session id should match start context");
            assertEquals(startContext.sid != null ? startContext.sid.toLowerCase() : null, outcome.sessionOwnerContext.sid != null ? outcome.sessionOwnerContext.sid.toLowerCase() : null, "downgrade owner-session SID should match start context SID");
            assertEquals(startContext.elev, outcome.sessionOwnerContext.elev, "downgrade owner-session elevated state should match start context");
        } finally {
            try {
                AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
            } catch (Throwable t) {
                logInfoAnyway("TestRunAsHelper: cleanup net user delete failed (AdminTestUser downgrade test): " + t.getMessage());
            }
        }
    }

    /**
     * {@link RunAsHelper#runInOwnerSession} must run the PowerShell probe in the same WTS session and as the same user SID as a normal
     * {@link ProcessBuilderFactory} launch from this JVM.
     */
    private void testRunInOwnerSessionRunsAsCurrentUserInCurrentSession() throws Exception {
        int sessionId = WindowsUtils.getCurrentProcessSessionId();
        if (sessionId < 0) {
            logInfoAnyway("TestRunAsHelper: no WTS session id, skip runInOwnerSession identity test.");
            return;
        }
        String expectSid = WindowsUtils.getCurrentUserSID();
        final ProbeResult baseline = runProbe(false);
        final ProbeResult viaHelper = runProbe(true);
        assertEquals(baseline, viaHelper, "runInOwnerSession probe should match ProcessBuilder baseline (session|sid|elev)");
        assertEquals(sessionId, viaHelper.session, "child WTS session from probe");
        assertTrue(expectSid != null && expectSid.equalsIgnoreCase(viaHelper.sid), "child SID from probe (expected " + expectSid + ")");
        ProcessOutput whoami = RunAsHelper.runInOwnerSession(new String[] { "cmd.exe", "/c", "whoami" }, RunAsLaunchOptions.DEFAULT);
        assertEquals(0, whoami.getExitCode(), "whoami exit");
        assertTrue(stderrEmpty(whoami), "whoami stderr must be empty: " + whoami.getErrOutString());
        String expectName = Advapi32Util.getUserName();
        assertTrue(whoami.getStdOutString().toLowerCase().contains(expectName.toLowerCase()), "whoami stdout should contain current user: " + whoami.getStdOutString());
    }

    private static String[] powershellSessionSidElevProbeArgv() {
        return new String[] { "powershell.exe", "-NoProfile", "-NonInteractive", "-Command", PS_PROBE_SESSION_SID_ELEV };
    }

    /**
     * Runs the session/sid/elev PowerShell probe in this JVM via {@link ProcessBuilderFactory} with the same checks as
     * {@link #fillOwnProbeViaGetCurrentContext(OwnerSessionWtsProbeOutcome)}.
     */
    private ProbeResult getCurrentContext() throws Exception {
        final OwnerSessionWtsProbeOutcome tmp = new OwnerSessionWtsProbeOutcome();
        fillOwnProbeViaGetCurrentContext(tmp);
        return tmp.ownContext;
    }

    /**
     * Fills {@code ret.own*} using the same probe and validation as {@link #getCurrentContext()}; for {@link ElevatedTestTask}
     * implementations that must not capture the outer test instance.
     */
    private static void fillOwnProbeViaGetCurrentContext(final OwnerSessionWtsProbeOutcome ret) throws Exception {
        ret.ownWindowsUtilsElevated = WindowsUtils.isElevated();
        final ProcessOutput pbProbe = ProcessBuilderFactory.runCommand(powershellSessionSidElevProbeArgv());
        ret.ownExitCode = pbProbe != null ? pbProbe.getExitCode() : -1;
        ret.ownStdout = pbProbe != null && pbProbe.getStdOutString() != null ? pbProbe.getStdOutString() : "";
        ret.ownStderr = pbProbe != null && pbProbe.getErrOutString() != null ? pbProbe.getErrOutString() : "";
        ret.ownContext = parseProbeFromProcessOutput(pbProbe);
    }

    /** Requires exit code 0, empty stderr, and parseable {@code session|sid|elev} first line. */
    private static ProbeResult parseProbeFromProcessOutput(final ProcessOutput out) throws IOException {
        if (out == null) {
            throw new IOException("probe: null ProcessOutput");
        }
        if (out.getExitCode() != 0) {
            throw new IOException("probe exit " + out.getExitCode() + ", stderr: " + out.getErrOutString());
        }
        if (!stderrEmpty(out)) {
            throw new IOException("probe stderr not empty: " + out.getErrOutString());
        }
        return parseProbe(out.getStdOutString());
    }

    private static void fillSessionProbeViaRunInOwnerSession(final OwnerSessionWtsProbeOutcome ret) throws Exception {
        final String[] probeCmd = powershellSessionSidElevProbeArgv();
        final ProcessOutput sessionOut = RunAsHelper.runInOwnerSession(probeCmd, RunAsLaunchOptions.DEFAULT);
        ret.sessionExitCode = sessionOut != null ? sessionOut.getExitCode() : -1;
        ret.sessionStdout = sessionOut != null && sessionOut.getStdOutString() != null ? sessionOut.getStdOutString() : "";
        ret.sessionStderr = sessionOut != null && sessionOut.getErrOutString() != null ? sessionOut.getErrOutString() : "";
        if (ret.sessionExitCode == 0 && stderrEmpty(sessionOut)) {
            ret.sessionOwnerContext = parseProbeFromProcessOutput(sessionOut);
        }
    }

    /**
     * Runs the PowerShell probe and returns {@link ProbeResult}. {@code viaRunInOwnerSession} selects {@link RunAsHelper#runInOwnerSession}
     * vs {@link ProcessBuilderFactory}; exit code must be 0.
     */
    private static ProbeResult runProbe(final boolean viaRunInOwnerSession) throws Exception {
        final String[] argv = powershellSessionSidElevProbeArgv();
        final ProcessOutput out = viaRunInOwnerSession ? RunAsHelper.runInOwnerSession(argv, RunAsLaunchOptions.DEFAULT) : ProcessBuilderFactory.runCommand(argv);
        return parseProbeFromProcessOutput(out);
    }

    private static String normalizePath(final String path) {
        if (path == null) {
            return null;
        }
        String p = path.trim().replace('/', '\\');
        while (p.endsWith("\\") && p.length() > 3) {
            p = p.substring(0, p.length() - 1);
        }
        return p.toLowerCase();
    }

    /**
     * Resolves SID for a local account created by this test. First tries plain name, then COMPUTERNAME\name.
     */
    private static String resolveLocalUserSid(final String userName) throws Exception {
        if (userName == null || userName.trim().length() == 0) {
            throw new IllegalArgumentException("userName cannot be empty");
        }
        try {
            final Advapi32Util.Account account = Advapi32Util.getAccountByName(userName.trim());
            if (account != null && account.sidString != null && account.sidString.trim().length() > 0) {
                return account.sidString.trim();
            }
        } catch (Throwable ignore) {
        }
        final String computer = System.getenv("COMPUTERNAME");
        if (computer != null && computer.trim().length() > 0) {
            final Advapi32Util.Account account = Advapi32Util.getAccountByName(computer.trim() + "\\" + userName.trim());
            if (account != null && account.sidString != null && account.sidString.trim().length() > 0) {
                return account.sidString.trim();
            }
        }
        throw new Exception("Could not resolve SID for local user: " + userName);
    }

    private static boolean stderrBlank(final String stderr) {
        return stderr == null || stderr.trim().length() == 0;
    }

    private static boolean stderrEmpty(final ProcessOutput out) {
        if (out == null) {
            return true;
        }
        return stderrBlank(out.getErrOutString());
    }

    /**
     * First line of stdout (trimmed), split by {@code '|'} into {@code session}, {@code sid}, {@code elev}. {@link ProbeResult#machineLine}
     * uses a lowercased SID for stable {@link ProbeResult#equals(Object)}.
     */
    private static ProbeResult parseProbe(final String stdout) throws IOException {
        if (stdout == null) {
            throw new IOException("probe: null stdout");
        }
        final String[] lines = stdout.split("\r?\n", -1);
        if (lines.length == 0) {
            throw new IOException("probe: empty stdout");
        }
        final String first = lines[0].trim();
        if (first.length() == 0) {
            throw new IOException("probe: first line empty");
        }
        final String[] parts = first.split("\\|", -1);
        if (parts.length != 3) {
            throw new IOException("probe: expected session|sid|elev, got: " + first);
        }
        final int session;
        try {
            session = Integer.parseInt(parts[0].trim());
        } catch (final NumberFormatException e) {
            throw new IOException("probe: bad session: " + parts[0], e);
        }
        final String sid = parts[1].trim();
        if (sid.length() == 0) {
            throw new IOException("probe: empty sid in first line");
        }
        final String elev = parts[2].trim();
        if (!"0".equals(elev) && !"1".equals(elev)) {
            throw new IOException("probe: elev must be 0 or 1, got: " + elev);
        }
        final String machineLine = session + "|" + sid.toLowerCase() + "|" + elev;
        return new ProbeResult(session, sid, elev, machineLine);
    }

    private void testRunAsUserWrongSidThrows() throws Exception {
        if (WindowsUtils.getCurrentProcessSessionId() < 0) {
            return;
        }
        try {
            RunAsHelper.runAsUser("S-1-5-21-99999999-9999999999-9999999999-999999", new String[] { "cmd.exe", "/c", "echo", "bad" }, RunAsLaunchOptions.DEFAULT);
            assertTrue(false, "expected IllegalStateException for SID mismatch");
        } catch (IllegalStateException e) {
            final String m = e.getMessage() != null ? e.getMessage().toLowerCase() : "";
            assertTrue(m.contains("sid") || m.contains("mismatch"), "expected SID-related error, got: " + e.getMessage());
        }
    }

    private void testRunUACElevatedRejectsEmptyCommand() throws Exception {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            return;
        }
        try {
            RunAsHelper.runUACElevated(new String[0], RunAsLaunchOptions.DEFAULT);
            assertTrue(false, "runUACElevated must reject empty cmd");
        } catch (IllegalArgumentException e) {
            logInfoAnyway("TestRunAsHelper: empty cmd rejected as expected: " + e.getMessage());
        }
    }

    /**
     * {@link RunAsHelper#runUACElevated} from an already elevated JVM must not return {@link ProcessOutput#getProcessInfo()} and must run
     * the child via owner-session launch.
     */
    private void testRunUACElevatedFromElevatedJvm() throws Exception {
        if (WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("TestRunAsHelper: skip runUACElevated-from-elevated (LocalSystem).");
            return;
        }
        if (WindowsUtils.isElevated()) {
            logInfoAnyway("TestRunAsHelper: skip nested runUACElevated-from-elevated (already elevated).");
            return;
        }
        AdminExecuter.runAsAdmin(new RunUACElevatedFromElevatedTask(), TypeRef.OBJECT, ProcessOptions.DEFAULT);
    }

    /**
     * UAC ShellExecuteEx launch with {@code waitFor=false}: elevated child, {@link JNAProcessInfo} on {@link ProcessOutput}, terminate via
     * {@link ProcessHandler}. Mirrors {@link org.appwork.utils.os.tests.TestWindowsUtils#testProcessManagement}.
     */
    private void testRunUACElevatedProcessManagement() throws Exception {
        if (WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("TestRunAsHelper: skip runUACElevated UAC test (LocalSystem).");
            return;
        }
        if (WindowsUtils.isElevated()) {
            logInfoAnyway("TestRunAsHelper: skip runUACElevated UAC test (already elevated; use testRunUACElevatedFromElevatedJvm).");
            return;
        }
        logInfoAnyway("TestRunAsHelper: will start elevated ping via RunAsHelper.runUACElevated (UAC consent required).");
        AdminExecuter.runAsAdmin(new ElevatedCheckTask(), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        final long started = Time.now();
        final RunAsLaunchOptions launchOpts = RunAsLaunchOptions.builder().waitFor(false).showWindow(false).build();
        final String[] cmd = new String[] { "cmd.exe", "/c", "ping", "-n", "100", "appwork.org" };
        JNAProcessInfo processInfo = null;
        try {
            processInfo = launchRunUACElevatedWithUacRetry(cmd, launchOpts);
        } catch (Win32Exception e) {
            logInfoAnyway("TestRunAsHelper: runUACElevated failed err=" + e.getErrorCode());
            throw e;
        }
        final ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
        try {
            assertTrue(processInfo.getPid() > 0, "pid must be positive");
            assertTrue(processInfo.getCommandLine() != null && processInfo.getCommandLine().length() > 0, "commandLine must be set");
            assertTrue(Time.now() - started > 1000, "UAC prompt should take some time");
            final List<ProcessInfo> stillRunning = handler.waitForExit(TimeSpan.fromMillis(2000), processInfo);
            assertTrue(stillRunning.size() > 0, "elevated child should still be running");
            final int pid = processInfo.getPid();
            assertEquals(1, ProcessHandlerFactory.getProcessHandler().listByPids(pid).size(), "process must exist");
            assertTrue(WindowsUtils.isProcessElevated(pid), "child must be elevated");
            Thread.sleep(1000);
            assertTrue(handler.terminateForced(processInfo, 5), "terminateForced must succeed");
            assertEquals(Integer.valueOf(5), WindowsUtils.waitForPID(pid, -1), "exit code after terminate");
            assertTrue(ProcessHandlerFactory.getProcessHandler().listByPids(pid).isEmpty(), "process must be gone");
        } finally {
            if (processInfo != null && !processInfo.isClosed()) {
                processInfo.close();
            }
        }
    }

    /**
     * {@link WindowsUtils#startElevatedProcess} must still return a valid {@link JNAProcessInfo} (delegates to
     * {@link RunAsHelper#runUACElevated}).
     */
    private void testWindowsUtilsStartElevatedProcessWrapper() throws Exception {
        if (WindowsUtils.isRunningAsLocalSystem() || WindowsUtils.isElevated()) {
            logInfoAnyway("TestRunAsHelper: skip startElevatedProcess wrapper UAC test.");
            return;
        }
        logInfoAnyway("TestRunAsHelper: will start cmd via WindowsUtils.startElevatedProcess (UAC consent required).");
        AdminExecuter.runAsAdmin(new ElevatedCheckTask(), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        final Timeout timeout = new Timeout(60 * 60000L);
        JNAProcessInfo processInfo = null;
        while (true) {
            try {
                processInfo = WindowsUtils.startElevatedProcess(new String[] { "cmd.exe", "/c", "ping", "-n", "100", "127.0.0.1" }, null, false);
                break;
            } catch (Win32Exception e) {
                if (timeout.isAlive() && e.getErrorCode() == ERROR_CANCELLED_UAC) {
                    logInfoAnyway("TestRunAsHelper: UAC cancelled (1223), retry startElevatedProcess...");
                    continue;
                }
                throw e;
            }
        }
        try {
            assertTrue(processInfo.getPid() > 0, "wrapper pid");
            assertTrue(processInfo.getCommandLine() != null && processInfo.getCommandLine().length() > 0, "wrapper commandLine");
            assertTrue(WindowsUtils.isProcessElevated(processInfo.getPid()), "wrapper child elevated");
            ProcessHandlerFactory.getProcessHandler().terminateForced(processInfo, 1);
        } finally {
            if (processInfo != null && !processInfo.isClosed()) {
                processInfo.close();
            }
        }
    }

    /**
     * {@link RunAsHelper#runUACElevated} with {@code waitFor=true} after UAC (short-lived child).
     */
    private void testRunUACElevatedWaitForExit() throws Exception {
        if (WindowsUtils.isRunningAsLocalSystem() || WindowsUtils.isElevated()) {
            logInfoAnyway("TestRunAsHelper: skip runUACElevated waitFor UAC test.");
            return;
        }
        logInfoAnyway("TestRunAsHelper: runUACElevated waitFor=true (UAC consent required).");
        AdminExecuter.runAsAdmin(new ElevatedCheckTask(), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        final RunAsLaunchOptions opts = RunAsLaunchOptions.builder().waitFor(true).showWindow(false).build();
        final Timeout timeout = new Timeout(60 * 60000L);
        ProcessOutput out = null;
        while (true) {
            try {
                out = RunAsHelper.runUACElevated(new String[] { "cmd.exe", "/c", "exit", "7" }, opts);
                break;
            } catch (Win32Exception e) {
                if (timeout.isAlive() && e.getErrorCode() == ERROR_CANCELLED_UAC) {
                    continue;
                }
                throw e;
            }
        }
        assertTrue(out != null, "ProcessOutput");
        assertTrue(out.getProcessInfo() == null, "waitFor=true ShellExecute path must not keep ProcessInfo");
        assertEquals(7, out.getExitCode(), "exit code");
    }

    private JNAProcessInfo launchRunUACElevatedWithUacRetry(String[] cmd, RunAsLaunchOptions options) throws Exception {
        final Timeout timeout = new Timeout(60 * 60000L);
        while (true) {
            try {
                final ProcessOutput out = RunAsHelper.runUACElevated(cmd, options);
                final ProcessInfo info = out.getProcessInfo();
                if (!(info instanceof JNAProcessInfo)) {
                    throw new Exception("runUACElevated(waitFor=false) must return JNAProcessInfo, got " + (info != null ? info.getClass().getName() : "null"));
                }
                final JNAProcessInfo jna = (JNAProcessInfo) info;
                assertTrue(out.getRemotePid() != null, "remotePid must be set");
                assertEquals(jna.getPid(), out.getRemotePid().intValue(), "remotePid must match JNAProcessInfo pid");
                return jna;
            } catch (Win32Exception e) {
                if (timeout.isAlive() && e.getErrorCode() == ERROR_CANCELLED_UAC) {
                    logInfoAnyway("TestRunAsHelper: UAC cancelled (1223), retry runUACElevated...");
                    continue;
                }
                throw e;
            }
        }
    }

    private void testRunAsUserMatchingSidWhoami() throws Exception {
        if (WindowsUtils.getCurrentProcessSessionId() < 0) {
            return;
        }
        String sid = WindowsUtils.getCurrentUserSID();
        ProcessOutput out = RunAsHelper.runAsUser(sid, new String[] { "cmd.exe", "/c", "whoami" }, RunAsLaunchOptions.DEFAULT);
        assertEquals(0, out.getExitCode(), "whoami exit");
        assertTrue(stderrEmpty(out), "whoami stderr must be empty: " + out.getErrOutString());
        String stdout = out.getStdOutString();
        String expect = Advapi32Util.getUserName();
        assertTrue(stdout != null && stdout.trim().length() > 0, "whoami stdout must be non-empty");
        assertTrue(stdout.toLowerCase().contains(expect.toLowerCase()), "whoami stdout should contain current user name: " + stdout);
    }

    private void testRunInOwnerSessionFromElevatedSameUser() throws Exception {
        if (WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("TestRunAsHelper: LocalSystem environment; skip elevated same-user scenario B.");
            return;
        }
        final ProbeResult baseline = getCurrentContext();
        if ("1".equals(baseline.elev)) {
            logInfoAnyway("TestRunAsHelper: current process already elevated; scenario B expects elevated->owner from helper, skipping nested check.");
            return;
        }
        final OwnerSessionWtsProbeOutcome outcome = AdminExecuter.runAsAdmin(new ElevatedSameUserOwnerSessionTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
        requireOwnOutcomeOk("elevated same-user B", outcome);
        requireSessionOutcomeOk("elevated same-user B session", outcome);
        assertTrue(outcome.ownWindowsUtilsElevated, "admin helper task must run elevated for scenario B (WindowsUtils.isElevated)");
        assertEquals(baseline.session, outcome.sessionOwnerContext.session, "owner-session probe must stay in the same interactive session");
        assertEquals(baseline.sid != null ? baseline.sid.toLowerCase() : null, outcome.sessionOwnerContext.sid != null ? outcome.sessionOwnerContext.sid.toLowerCase() : null, "owner-session probe must keep same user SID");
        assertEquals("0", outcome.sessionOwnerContext.elev, "owner-session probe should de-elevate back to interactive token");
    }

    /**
     * Creates a local user as admin and runs only the PowerShell probe under that user (without {@link RunAsHelper} inside the task) to
     * validate that runAsUser itself works and that probe output can be parsed/evaluated.
     */
    private void testRunAsAnotherUser() throws Exception {
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("TestRunAsHelper: skip cross-user test (no session id).");
            return;
        }
        final String userName = "TestUser";
        final String password = Crypto.generateRandomString(10, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(userName, password, false), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final ProbeResult baseline = getCurrentContext();
            final String expectedTestUserSid = resolveLocalUserSid(userName);
            final OwnerSessionWtsProbeOutcome outcome = AdminExecuter.runAsUser(null, userName, password, new CrossUserSimpleProbeTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
            requireOwnOutcomeOk("cross-user", outcome);
            assertFalse(outcome.ownWindowsUtilsElevated, "cross-user: standard local user must not be elevated (WindowsUtils.isElevated)");
            assertEquals(interactiveSessionId, outcome.ownContext.session, "cross-user own probe should run in the same WTS session");
            assertEquals(expectedTestUserSid != null ? expectedTestUserSid.toLowerCase() : null, outcome.ownContext.sid != null ? outcome.ownContext.sid.toLowerCase() : null, "cross-user own probe SID should match created test user SID");
            assertTrue(!baseline.sid.equalsIgnoreCase(outcome.ownContext.sid), "cross-user own probe SID should differ from interactive baseline SID");
        } finally {
            try {
                AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
            } catch (Throwable t) {
                logInfoAnyway("TestRunAsHelper: cleanup net user delete failed: " + t.getMessage());
            }
        }
    }

    /**
     * Explicit owner-context roundtrip:
     * <ol>
     * <li>Start task as test user.</li>
     * <li>Task resolves session owner.</li>
     * <li>Task reads owner RoamingAppData via owner token.</li>
     * <li>Original user compares returned AppData with own AppData.</li>
     * </ol>
     */
    public void testRunAsUserOwnerAppDataRoundtrip() throws Exception {
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("TestRunAsHelper: skip owner-appdata roundtrip (no session id).");
            return;
        }
        final String userName = "OwnerAppDataUser";
        final String password = Crypto.generateRandomString(10, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(userName, password, true), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final ProbeResult startContext = getCurrentContext();
            final String ownRoamingAppData = normalizePath(RunAsHelper.getKnownFolderPath(null, KnownFolders.FOLDERID_RoamingAppData, ShlObj.KNOWN_FOLDER_FLAG.NONE.getFlag()));
            final OwnerSessionWtsProbeOutcome outcome = AdminExecuter.runAsUser(null, userName, password, new CrossUserOwnerAppDataTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
            requireOwnOutcomeOk("owner-appdata-roundtrip", outcome);
            assertTrue(outcome.ownerSidInTask != null && outcome.ownerSidInTask.trim().length() > 0, "owner-appdata-roundtrip: task must resolve owner sid");
            assertEquals(startContext.sid != null ? startContext.sid.toLowerCase() : null, outcome.ownerSidInTask != null ? outcome.ownerSidInTask.toLowerCase() : null, "owner-appdata-roundtrip: owner sid in task must match original user sid");
            final String returnedOwnerAppData = normalizePath(outcome.sessionOwnerRoamingAppData);
            assertTrue(returnedOwnerAppData != null && returnedOwnerAppData.length() > 0, "owner-appdata-roundtrip: returned owner appdata must be present");
            assertEquals(ownRoamingAppData, returnedOwnerAppData, "owner-appdata-roundtrip: returned owner appdata must match original user appdata");
        } finally {
            try {
                AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
            } catch (Throwable t) {
                logInfoAnyway("TestRunAsHelper: cleanup net user delete failed (owner-appdata roundtrip): " + t.getMessage());
            }
        }
    }

    /** Serializable: must not capture the outer {@link AWTest} instance (see {@link AdminExecuter} task serialization). */
    private static final class ElevatedSameUserOwnerSessionTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome ret = new OwnerSessionWtsProbeOutcome();
            fillOwnProbeViaGetCurrentContext(ret);
            fillSessionProbeViaRunInOwnerSession(ret);
            return ret;
        }
    }

    /**
     * Ensures a fresh local account: if {@code net user name} succeeds, runs {@code net user name /delete} (must succeed), then
     * {@code net user name password /add ...}.
     */
    private static final class CreateTestWindowsUserTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      userName;
        private final String      password;
        private final boolean     addToAdministrators;

        CreateTestWindowsUserTask(String userName, String password, boolean addToAdministrators) {
            this.userName = userName != null ? userName : "";
            this.password = password != null ? password : "";
            this.addToAdministrators = addToAdministrators;
        }

        @Override
        public Serializable run() throws Exception {
            if (userName.length() == 0) {
                throw new Exception("CreateTestWindowsUserTask: empty userName");
            }
            final ProcessOutput existsProbe = ProcessBuilderFactory.runCommand("net", "user", userName);
            if (existsProbe.getExitCode() == 0) {
                final ProcessOutput del = ProcessBuilderFactory.runCommand("net", "user", userName, "/delete");
                if (del.getExitCode() != 0) {
                    throw new Exception("net user /delete failed for existing user '" + userName + "': " + del.getErrOutString() + " " + del.getStdOutString());
                }
            }
            final ProcessOutput add = ProcessBuilderFactory.runCommand("net", "user", userName, password, "/add", "/expires:never", "/passwordchg:no");
            if (add.getExitCode() != 0) {
                throw new Exception("net user add failed: " + add.getErrOutString() + " " + add.getStdOutString());
            }
            if (addToAdministrators) {
                // Locale-independent: resolve built-in Administrators by well-known SID (e.g. "Administratoren" on German Windows).
                final String builtinAdminSid = WindowsUtils.SID.SID_BUILTIN_ADMINISTRATORS.sid;
                final Advapi32Util.Account adminGroup = Advapi32Util.getAccountBySid(builtinAdminSid);
                final String localAdminGroupName = adminGroup.name;
                final ProcessOutput groupAdd = ProcessBuilderFactory.runCommand("net", "localgroup", localAdminGroupName, userName, "/add");
                if (groupAdd.getExitCode() != 0) {
                    throw new Exception("net localgroup add failed for '" + userName + "' to built-in administrators (SID " + builtinAdminSid + ", resolved name '" + localAdminGroupName + "'): " + groupAdd.getErrOutString() + " " + groupAdd.getStdOutString());
                }
            }
            return Boolean.TRUE;
        }
    }

    private static final class DeleteTestWindowsUserTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      userName;

        DeleteTestWindowsUserTask(String userName) {
            this.userName = userName != null ? userName : "";
        }

        @Override
        public Serializable run() throws Exception {
            ProcessBuilderFactory.runCommand("net", "user", userName, "/delete");
            return null;
        }
    }

    /** Runs as test user and executes only the local PowerShell probe (no RunAsHelper-in-task dependency). */
    private static final class CrossUserSimpleProbeTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome ret = new OwnerSessionWtsProbeOutcome();
            fillOwnProbeViaGetCurrentContext(ret);
            return ret;
        }
    }

    /**
     * Runs only {@link #fillSessionProbeViaRunInOwnerSession} in a JVM that was elevated for the <strong>current</strong> Windows user via
     * {@link AdminExecuter#runAsAdmin} (ShellExecuteEx {@code runas} from the {@code RUN_AS_USER} task process). Used from
     * {@link CrossUserDowngradeToOwnerTask} because the filtered {@code runAsUser} token cannot call {@link RunAsHelper#runInOwnerSession}.
     */
    private static final class OwnerSessionProbeViaAdminHelperTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome r = new OwnerSessionWtsProbeOutcome();
            fillSessionProbeViaRunInOwnerSession(r);
            InteractiveSessionOwner owner = null;
            try {
                owner = RunAsHelper.resolveInteractiveOwnerForCurrentProcessStrict();
                if (owner != null) {
                    r.sessionOwnerRoamingAppData = RunAsHelper.getKnownFolderPath(owner.getUserTokenHandle(), KnownFolders.FOLDERID_RoamingAppData, ShlObj.KNOWN_FOLDER_FLAG.NONE.getFlag());
                }
            } finally {
                if (owner != null) {
                    owner.close();
                }
            }
            return r;
        }
    }

    /**
     * Runs as test user: own probe in the {@code runAsUser} JVM; owner-session probe via nested {@link AdminExecuter#runAsAdmin} (elevates
     * the same user with {@link org.appwork.utils.os.WindowsUtils#startElevatedProcess}, then {@link RunAsHelper#runInOwnerSession} there),
     * then merges {@code session*} fields into the returned outcome.
     */
    private static final class CrossUserDowngradeToOwnerTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome ret = new OwnerSessionWtsProbeOutcome();
            fillOwnProbeViaGetCurrentContext(ret);
            try {
                final OwnerSessionWtsProbeOutcome adminProbe = AdminExecuter.runAsAdmin(new OwnerSessionProbeViaAdminHelperTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
                if (adminProbe != null) {
                    ret.sessionOwnerContext = adminProbe.sessionOwnerContext;
                    ret.sessionExitCode = adminProbe.sessionExitCode;
                    ret.sessionStdout = adminProbe.sessionStdout;
                    ret.sessionStderr = adminProbe.sessionStderr;
                }
            } catch (Throwable t) {
                ret.sessionExitCode = -1;
                ret.sessionStderr = Exceptions.getStackTrace(t);
                ret.sessionStdout = "";
                ret.sessionOwnerContext = null;
            }
            return ret;
        }
    }

    /**
     * Runs as test user, resolves session owner via RunAsHelper, and returns owner RoamingAppData path.
     */
    private static final class CrossUserOwnerAppDataTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome ret = new OwnerSessionWtsProbeOutcome();
            fillOwnProbeViaGetCurrentContext(ret);
            try {
                final OwnerSessionWtsProbeOutcome elevated = AdminExecuter.runAsAdmin(new OwnerAppDataViaAdminHelperTask(), TYPE_OWNER_SESSION_WTS_PROBE_OUTCOME, ProcessOptions.DEFAULT);
                if (elevated != null) {
                    ret.ownerSidInTask = elevated.ownerSidInTask;
                    ret.sessionOwnerRoamingAppData = elevated.sessionOwnerRoamingAppData;
                }
            } catch (Throwable t) {
                ret.sessionStderr = Exceptions.getStackTrace(t);
                ret.sessionExitCode = -1;
            }
            return ret;
        }
    }

    /**
     * Runs elevated as the same user and resolves owner SID + owner RoamingAppData.
     */
    private static final class OwnerAppDataViaAdminHelperTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final OwnerSessionWtsProbeOutcome ret = new OwnerSessionWtsProbeOutcome();
            InteractiveSessionOwner owner = null;
            try {
                owner = RunAsHelper.resolveInteractiveOwnerForCurrentProcessStrict();
                if (owner != null) {
                    ret.ownerSidInTask = owner.getOwnerSid();
                    ret.sessionOwnerRoamingAppData = RunAsHelper.getKnownFolderPath(owner.getUserTokenHandle(), KnownFolders.FOLDERID_RoamingAppData, ShlObj.KNOWN_FOLDER_FLAG.NONE.getFlag());
                }
            } finally {
                if (owner != null) {
                    owner.close();
                }
            }
            return ret;
        }
    }
}
