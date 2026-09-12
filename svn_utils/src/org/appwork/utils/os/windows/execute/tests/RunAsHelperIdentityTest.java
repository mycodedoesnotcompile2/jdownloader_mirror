/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute.tests;

import java.io.Serializable;
import java.util.EnumSet;
import java.util.Set;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.TestTag;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.Exceptions;
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.InteractiveSessionOwner;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.os.windows.execute.RunAsWin32ApiTrace;
import org.appwork.utils.os.windows.execute.SessionUserTokens;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HWND;

/**
 * Identity tests for {@link RunAsHelper} only: WTS session id, user SID, high-integrity (elevation) flag, and {@code whoami} text from
 * processes started via {@link RunAsHelper#runInOwnerSession} / {@link RunAsHelper#runInSession} / {@link RunAsHelper#runAsUser}.
 * <p>
 * {@link AdminExecuter} is used only to place the launching JVM in SYSTEM / elevated / other-user JVMs; pass/fail rules target
 * {@link RunAsHelper} behaviour, not AdminExecuter internals.
 * <p>
 * Before cross-account scenarios, this test captures {@link InteractiveOwnerBaseline} from the host interactive session (session id,
 * {@code ownerSid}, elevation flag). Session-owner children must match that baseline (WTS session, SID, non-elevated integrity).
 * <p>
 * Scenario overview:
 *
 * <pre>
 * 00 Host runInOwnerSession (same interactive owner, not elevated)
 *    Step 1: Skip if host is elevated or Local System
 *    Step 2: runInOwnerSession from this JVM
 *    Step 3: We expect session-owner SID/WTS to match InteractiveOwnerBaseline
 *
 * Host runAsUser SID guards (no AdminExecuter)
 *    Step 1: runAsUser with bogus expectedSid must throw
 *    Step 2: runAsUser with baseline.ownerSid must succeed
 *
 * 01 Local System -&gt; session owner (runInSession with explicit interactive session id)
 *    Step 1: Execute task as Local System
 *    Step 2: Launch session-owner identity probe via runInSession(interactiveSessionId)
 *    Step 3: We expect session-owner SID to match InteractiveOwnerBaseline (not S-1-5-18)
 *
 * 01b Local System -&gt; session owner (runInOwnerSession — installer / Software Center / PsExec -s path)
 *    Step 1: Execute task as Local System (helper JVM is in session 0)
 *    Step 2: resolveOwnerSessionIdForCurrentProcess must map session 0 -&gt; interactive owner (Explorer / WTSActive+winStation, not merely active console); then runInOwnerSession
 *    Step 3: We expect session-owner SID/WTS to match InteractiveOwnerBaseline
 *
 * 02 Elevated admin -&gt; session owner (runInOwnerSession)
 *    Step 1: Execute task as elevated admin
 *    Step 2: Launch session-owner identity probe
 *    Step 3: We expect session-owner SID to match InteractiveOwnerBaseline
 *
 * 03 Plain test user -&gt; session owner
 *    Step 1: Create non-admin test user
 *    Step 2: Execute task as that user (not elevated)
 *    Step 3: We expect runInOwnerSession to FAIL (privilege)
 *
 * 04 Admin-capable test user -&gt; session owner
 *    Step 1: Create admin-capable test user
 *    Step 2: Execute task as that user (not elevated); runInOwnerSession must FAIL
 *    Step 3: Nested elevated-admin task; session-owner probe must match InteractiveOwnerBaseline
 * </pre>
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperIdentityTest extends AWTest {
    private static final String                        SID_LOCAL_SYSTEM            = "S-1-5-18";
    /**
     * Machine line {@code session|sid|elev} from a <strong>powershell.exe child</strong> ({@code elev} {@code 1} = high mandatory label
     * S-1-16-12288 in that child). This can differ from {@link WindowsUtils#isElevated()} ({@code TokenIsElevated}) on the host JVM when an
     * elevated Java process spawns a medium-integrity probe. Use {@link IdentityJvmSnapshot#windowsUtilsElevated} for launching JVM
     * elevation; use this probe for session owner children launched via {@link RunAsHelper}.
     */
    private static final String                        PS_PROBE_SESSION_SID_ELEV   = "$s=[int]([Diagnostics.Process]::GetCurrentProcess().SessionId);" + "$id=[System.Security.Principal.WindowsIdentity]::GetCurrent().User.Value;" + "$pr=New-Object System.Security.Principal.WindowsPrincipal([System.Security.Principal.WindowsIdentity]::GetCurrent());" + "$hi=$pr.IsInRole([System.Security.Principal.SecurityIdentifier]::new('S-1-16-12288'));" + "$e=if($hi){'1'}else{'0'};" + "Write-Output ($s.ToString()+'|'+$id+'|'+$e);" + "Write-Output ('session='+$s);Write-Output ('sid='+$id);Write-Output ('elev='+$e)";
    private static final String                        USER_PLAIN                  = "RunAsEnvPlainUser";
    private static final String                        USER_ADMIN_CAPABLE          = "RunAsEnvTestUser";
    private static final TypeRef<IdentityJvmSnapshot>  TYPE_IDENTITY_JVM_SNAPSHOT  = new TypeRef<IdentityJvmSnapshot>() {
                                                                                   };
    private static final TypeRef<NestedIdentityReport> TYPE_NESTED_IDENTITY_REPORT = new TypeRef<NestedIdentityReport>() {
                                                                                   };

    public static void main(String[] args) {
        run();
    }

    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public Set<TestTag> getTags() {
        return EnumSet.of(TestTag.UAC);
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperIdentityTest: Windows + JNA required, skipped.");
            return;
        }
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("RunAsHelperIdentityTest: no interactive WTS session id, skipped.");
            return;
        }
        final InteractiveOwnerBaseline baseline = captureInteractiveOwnerBaseline(interactiveSessionId);
        logInfoAnyway("RunAsHelperIdentityTest: interactive baseline " + baseline.machineLine);
        // Scenario 00 + host guards
        testHostRunInOwnerSessionWhenSameUserNotElevated(baseline);
        testHostRunAsUserSidGuards(baseline);
        test01LocalSystemToSessionOwnerIdentity(baseline, interactiveSessionId);
        test01bLocalSystemToSessionOwnerViaRunInOwnerSession(baseline, interactiveSessionId);
        test02ElevatedAdminToSessionOwnerIdentity(baseline);
        test03PlainTestUserCannotLaunchSessionOwnerIdentity();
        test04AdminCapableTestUserToSessionOwnerIdentity(baseline);
    }

    /**
     * Scenario 00: {@link RunAsHelper#runInOwnerSession} from this JVM when already the interactive owner and not elevated.
     */
    private void testHostRunInOwnerSessionWhenSameUserNotElevated(final InteractiveOwnerBaseline baseline) throws Exception {
        if (WindowsUtils.isElevated()) {
            logInfoAnyway("RunAsHelperIdentityTest: skip host runInOwnerSession (already elevated).");
            return;
        }
        if (WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("RunAsHelperIdentityTest: skip host runInOwnerSession (LocalSystem host).");
            return;
        }
        final IdentityJvmSnapshot snapshot = new IdentityJvmSnapshot();
        fillLaunchingJvmIdentity(snapshot);
        // Step 2: Launch session-owner identity probe from host JVM
        launchSessionOwnerIdentityProbe(snapshot, OwnerLaunchMode.CURRENT_SESSION, -1);
        // Step 3: We expect session-owner SID to match InteractiveOwnerBaseline
        assertSessionOwnerMatchesInteractiveBaseline("00-host-runInOwnerSession", snapshot, baseline);
    }

    /** Host {@link RunAsHelper#runAsUser} SID guard checks (no AdminExecuter). */
    private void testHostRunAsUserSidGuards(final InteractiveOwnerBaseline baseline) throws Exception {
        try {
            RunAsHelper.runAsUser("S-1-5-21-99999999-9999999999-9999999999-999999", new String[] { "cmd.exe", "/c", "exit", "0" }, RunAsLaunchOptions.DEFAULT);
            // Bogus expectedSid must be rejected before any child process is started.
            assertTrue(false, "runAsUser must reject unknown expectedSid");
        } catch (IllegalStateException e) {
            final String m = e.getMessage() != null ? e.getMessage().toLowerCase() : "";
            // Rejection reason should mention SID / mismatch so we know it failed for the guard, not elsewhere.
            assertTrue(m.indexOf("sid") >= 0 || m.indexOf("mismatch") >= 0, "runAsUser wrong sid: " + e.getMessage());
        }
        final ProcessOutput whoami = RunAsHelper.runAsUser(baseline.ownerSid, new String[] { "cmd.exe", "/c", "whoami" }, RunAsLaunchOptions.DEFAULT);
        // Matching baseline ownerSid must allow launch; whoami exit 0 proves the child ran.
        assertEquals(0, whoami.getExitCode(), "runAsUser matching sid whoami exit");
        final String out = whoami.getStdOutString() != null ? whoami.getStdOutString() : "";
        final String expectName = Advapi32Util.getUserName();
        // Child must run as the interactive host user named by the matching SID guard.
        assertTrue(out.toLowerCase().indexOf(expectName.toLowerCase()) >= 0, "runAsUser whoami should contain " + expectName + ", got: " + out);
    }

    /** Scenario 01: Local System helper JVM; session-owner identity via {@link RunAsHelper#runInSession}. */
    private void test01LocalSystemToSessionOwnerIdentity(final InteractiveOwnerBaseline baseline, final int interactiveSessionId) throws Exception {
        // Step 1: Execute task as Local System
        final IdentityJvmSnapshot snapshot = AdminExecuter.runAsLocalSystem(new LocalSystemToSessionOwnerIdentityTask(interactiveSessionId), TYPE_IDENTITY_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        // Parent must be LocalSystem — otherwise runInSession is not exercised from S-1-5-18.
        assertTrue(snapshot.isLocalSystem, "01: launching JVM must be LocalSystem");
        // Probe SID must be SYSTEM; session/elev left unchecked (SYSTEM context varies by service session).
        assertIdentityLineEquals("01/launchingJvm", snapshot.launchingJvmIdentity, SID_LOCAL_SYSTEM, null, null);
        // Steps 2–3: session-owner probe must match InteractiveOwnerBaseline
        assertSessionOwnerMatchesInteractiveBaseline("01-local-system->sessionOwner", snapshot, baseline);
    }

    /**
     * Scenario 01b: same as 01, but via {@link RunAsHelper#runInOwnerSession} (installer path under LocalSystem/session 0). Covers
     * {@link InteractiveSessionOwner#resolveOwnerSessionIdForCurrentProcess()} redirect to the interactive owner session (Explorer /
     * WTSActive+winStation, same idea as ConnectService HTTPHandler.handleSelfTest).
     */
    private void test01bLocalSystemToSessionOwnerViaRunInOwnerSession(final InteractiveOwnerBaseline baseline, final int interactiveSessionId) throws Exception {
        final IdentityJvmSnapshot snapshot = AdminExecuter.runAsLocalSystem(new LocalSystemToSessionOwnerViaRunInOwnerSessionIdentityTask(interactiveSessionId), TYPE_IDENTITY_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        assertTrue(snapshot.isLocalSystem, "01b: launching JVM must be LocalSystem");
        assertIdentityLineEquals("01b/launchingJvm", snapshot.launchingJvmIdentity, SID_LOCAL_SYSTEM, null, null);
        assertEquals(interactiveSessionId, snapshot.resolvedOwnerSessionId, "01b: resolveOwnerSessionIdForCurrentProcess must map LocalSystem/session0 to interactive owner session");
        assertSessionOwnerMatchesInteractiveBaseline("01b-local-system->runInOwnerSession", snapshot, baseline);
    }

    /** Scenario 02: Elevated admin helper JVM; session-owner identity via {@link RunAsHelper#runInOwnerSession}. */
    private void test02ElevatedAdminToSessionOwnerIdentity(final InteractiveOwnerBaseline baseline) throws Exception {
        // Step 1: Execute task as elevated admin
        final IdentityJvmSnapshot snapshot = AdminExecuter.runAsAdmin(new ElevatedAdminToSessionOwnerIdentityTask(OwnerLaunchMode.CURRENT_SESSION, -1), TYPE_IDENTITY_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        // Parent must be TokenIsElevated — RunAsHelper session-owner path needs elevation privileges from this JVM.
        assertTrue(snapshot.windowsUtilsElevated, "02: elevated helper JVM (TokenIsElevated; RunAsHelper uses this, not PS integrity probe)");
        // Session-owner child must match InteractiveOwnerBaseline (not the elevated admin identity).
        assertSessionOwnerMatchesInteractiveBaseline("02-elevated-admin->sessionOwner", snapshot, baseline);
    }

    /** Scenario 03: Plain non-admin user must not reach session-owner launch (privilege). */
    private void test03PlainTestUserCannotLaunchSessionOwnerIdentity() throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        // Step 1: Create non-admin test user
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_PLAIN, password, false), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            // Step 2: Execute task as plain test user (not elevated)
            final IdentityJvmSnapshot snapshot = AdminExecuter.runAsUser(null, USER_PLAIN, password, new PlainTestUserToSessionOwnerIdentityTask(OwnerLaunchMode.CURRENT_SESSION), TYPE_IDENTITY_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
            // Step 3: We expect runInOwnerSession to FAIL
            assertPlainTestUserSessionOwnerLaunchRejected("03-plain-testuser->sessionOwner", snapshot);
        } finally {
            deleteTestUserQuietly(USER_PLAIN);
        }
    }

    /** Scenario 04: Admin-capable non-elevated user fails session-owner launch; nested elevated admin matches baseline. */
    private void test04AdminCapableTestUserToSessionOwnerIdentity(final InteractiveOwnerBaseline baseline) throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        // Step 1: Create admin-capable test user
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_ADMIN_CAPABLE, password, true), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            // Step 2–3: runAsUser task + nested elevated-admin identity probe
            final NestedIdentityReport report = AdminExecuter.runAsUser(null, USER_ADMIN_CAPABLE, password, new AdminCapableTestUserToSessionOwnerIdentityTask(), TYPE_NESTED_IDENTITY_REPORT, ProcessOptions.DEFAULT);
            // Non-elevated admin-capable user must fail; nested elevated admin must match InteractiveOwnerBaseline.
            assertNestedSessionOwnerIdentityReport("04-admin-capable-testuser->sessionOwner", report, baseline);
        } finally {
            deleteTestUserQuietly(USER_ADMIN_CAPABLE);
        }
    }

    private static InteractiveOwnerBaseline captureInteractiveOwnerBaseline(final int interactiveSessionId) throws Exception {
        final ProcessOutput own = ProcessBuilderFactory.runCommand(powershellIdentityArgv());
        final ParsedIdentityLine line = parseIdentityProbeOutput("host-baseline", own);
        final InteractiveOwnerBaseline b = new InteractiveOwnerBaseline();
        b.interactiveSessionId = interactiveSessionId;
        b.ownerSid = line.sid;
        b.ownerElev = line.elev;
        b.machineLine = line.machineLine;
        return b;
    }

    private void assertSessionOwnerMatchesInteractiveBaseline(final String label, final IdentityJvmSnapshot snapshot, final InteractiveOwnerBaseline baseline) throws Exception {
        // Snapshot must exist — otherwise session-owner launch did not return identity data.
        assertTrue(snapshot != null, label + ": snapshot null");
        // Identity powershell probe must succeed so we can compare session|sid|elev to the baseline.
        assertIdentityProbeOk(label + "/sessionOwner", snapshot.sessionOwnerIdentityProbe);
        final ParsedIdentityLine sessionOwner = snapshot.sessionOwnerIdentityParsed;
        // Child must stay in the host interactive WTS session (not a service/console switch).
        assertEquals(baseline.interactiveSessionId, sessionOwner.session, label + ": WTS session");
        // Child SID must be the interactive session owner captured before AdminExecuter scenarios.
        assertEquals(baseline.ownerSid, sessionOwner.sid, label + ": session owner SID");
        // Session owner is a medium-integrity interactive user — high integrity would mean elevation leaked.
        assertEquals("0", sessionOwner.elev, label + ": session owner child must not be high integrity");
        // SYSTEM is never the interactive owner; that would mean we failed to leave LocalSystem context.
        assertTrue(!SID_LOCAL_SYSTEM.equalsIgnoreCase(sessionOwner.sid), label + ": session owner child must not be SYSTEM (" + sessionOwner.sid + ")");
        // Combined line check: SID + WTS session + non-elevated integrity must all match the baseline.
        assertIdentityLineEquals(label + "/sessionOwner", sessionOwner, baseline.ownerSid, Integer.valueOf(baseline.interactiveSessionId), "0");
        // whoami /user must also succeed as a second, independent identity signal.
        assertWhoamiProbeOk(label + "/sessionOwnerWhoami", snapshot.sessionOwnerWhoamiProbe);
        // whoami must not report SYSTEM — confirms we did not launch as S-1-5-18.
        assertTrue(!stdoutContainsSid(snapshot.sessionOwnerWhoamiProbe, SID_LOCAL_SYSTEM), label + ": session owner whoami must not contain SYSTEM sid, stdout=" + snapshot.sessionOwnerWhoamiProbe.stdout);
        if (baseline.ownerSid != null && baseline.ownerSid.length() > 0) {
            // whoami stdout should reference the same baseline owner SID as the powershell probe.
            assertTrue(stdoutContainsSid(snapshot.sessionOwnerWhoamiProbe, baseline.ownerSid), label + ": session owner whoami should reference baseline sid, stdout=" + snapshot.sessionOwnerWhoamiProbe.stdout);
        }
    }

    private void assertNestedSessionOwnerIdentityReport(final String label, final NestedIdentityReport report, final InteractiveOwnerBaseline baseline) throws Exception {
        // Nested report must exist — outer runAsUser task must have returned both legs.
        assertTrue(report != null, label + ": report null");
        // First leg: snapshot from the admin-capable but non-elevated test user JVM.
        assertTrue(report.adminCapableTestUserSnapshot != null, label + ": adminCapableTestUserSnapshot missing");
        // runAsUser path must not elevate — we need the privilege-denied case for runInOwnerSession.
        assertFalse(report.adminCapableTestUserSnapshot.windowsUtilsElevated, label + ": runAsUser must not be elevated");
        // Launching JVM identity probe must succeed so we can prove it is not the interactive owner.
        assertIdentityProbeOk(label + "/runAsUser/launchingJvm", report.adminCapableTestUserSnapshot.launchingJvmIdentityProbe);
        // Test user SID must differ from InteractiveOwnerBaseline — otherwise the fail/success contrast is meaningless.
        assertTrue(!baseline.ownerSid.equalsIgnoreCase(report.adminCapableTestUserSnapshot.launchingJvmIdentityParsed.sid), label + ": runAsUser sid must differ from interactive owner");
        // Step 2: Admin-capable but non-elevated runAsUser JVM must fail runInOwnerSession (same checks as scenario 03)
        assertPlainTestUserSessionOwnerLaunchRejected(label + "/adminCapableTestUser", report.adminCapableTestUserSnapshot);
        // Nested elevated-admin leg must have produced a snapshot (no silent nested failure).
        assertTrue(report.elevatedAdminSnapshot != null, label + ": elevatedAdminSnapshot missing, nested:\n" + report.nestedFailureStack);
        // Nested AdminExecuter.runAsAdmin must not have thrown — stack empty means elevation + probe ran.
        assertTrue(report.nestedFailureStack == null || report.nestedFailureStack.trim().length() == 0, label + ": nested:\n" + report.nestedFailureStack);
        // Step 3: Nested elevated-admin session-owner probe must match InteractiveOwnerBaseline
        assertSessionOwnerMatchesInteractiveBaseline(label + "/elevated", report.elevatedAdminSnapshot, baseline);
    }

    private void assertPlainTestUserSessionOwnerLaunchRejected(final String label, final IdentityJvmSnapshot snapshot) throws Exception {
        // Snapshot must exist — privilege-denied path still returns captured probe data.
        assertTrue(snapshot != null, label + ": snapshot null");
        // Caller must not be elevated — elevated users can succeed and would invalidate this negative test.
        assertFalse(snapshot.windowsUtilsElevated, label + ": plain user not elevated");
        // Launching JVM probe must succeed so we know the failure is specific to session-owner launch.
        assertIdentityProbeOk(label + "/launchingJvm", snapshot.launchingJvmIdentityProbe);
        // Session-owner probe object must exist even when launch fails (exit/stderr captured).
        assertTrue(snapshot.sessionOwnerIdentityProbe != null, label + ": sessionOwner probe null");
        // runInOwnerSession must fail — non-elevated / insufficient privilege cannot impersonate session owner.
        assertTrue(snapshot.sessionOwnerIdentityProbe.exitCode != 0, label + ": sessionOwner launch must fail, exit=" + snapshot.sessionOwnerIdentityProbe.exitCode);
        // Failure must leave stderr — empty stderr would look like a silent/unexpected abort.
        assertTrue(!stderrBlank(snapshot.sessionOwnerIdentityProbe.stderr), label + ": stderr empty");
        // Stderr must indicate access denied / missing privilege / RunAsHelper reject, not an unrelated crash.
        assertTrue(stderrIndicatesLaunchRejected(snapshot.sessionOwnerIdentityProbe.stderr), label + ": stderr=" + snapshot.sessionOwnerIdentityProbe.stderr);
    }

    private void assertIdentityProbeOk(final String label, final IdentityProbeRun run) throws Exception {
        // Probe run must exist — missing means launch did not capture ProcessOutput.
        assertTrue(run != null, label + ": probe null");
        // Exit 0 required so session|sid|elev on stdout is trustworthy.
        assertEquals(0, run.exitCode, label + ": exit, stderr=" + run.stderr + ", stdout=" + run.stdout);
        // Clean stderr — PowerShell warnings/errors would make the identity line unreliable.
        assertTrue(stderrBlank(run.stderr), label + ": stderr=" + run.stderr);
        // First stdout line must hold the machine-readable session|sid|elev triple.
        assertTrue(run.firstLine != null && run.firstLine.length() > 0, label + ": firstLine missing");
    }

    private void assertWhoamiProbeOk(final String label, final IdentityProbeRun run) throws Exception {
        // whoami probe must exist — second channel for SID verification.
        assertTrue(run != null, label + ": whoami probe null");
        // whoami must succeed; non-zero exit means we cannot cross-check the powershell SID.
        assertEquals(0, run.exitCode, label + ": whoami exit, stderr=" + run.stderr);
        // Non-empty stdout required so SID/user text can be searched.
        assertTrue(run.stdout != null && run.stdout.trim().length() > 0, label + ": whoami stdout empty");
    }

    /**
     * @param expectSid
     *            optional exact SID (lower case compare); {@code null} = skip
     * @param expectSession
     *            optional session id; {@code null} = skip
     * @param expectElev
     *            optional {@code "0"} or {@code "1"}; {@code null} = skip
     */
    private void assertIdentityLineEquals(final String label, final ParsedIdentityLine line, final String expectSid, final Integer expectSession, final String expectElev) throws Exception {
        // Parsed identity triple must exist before optional field checks.
        assertTrue(line != null, label + ": identity line null");
        if (expectSid != null) {
            // SID must match the expected account for this launch path.
            assertEquals(expectSid.toLowerCase(), line.sid, label + ": sid");
        }
        if (expectSession != null) {
            // WTS session id must match when the scenario pins an interactive session.
            assertEquals(expectSession.intValue(), line.session, label + ": session");
        }
        if (expectElev != null) {
            // High-integrity flag must match (0 = medium/lower, 1 = elevated).
            assertEquals(expectElev, line.elev, label + ": elev");
        }
    }

    private static boolean stderrBlank(final String stderr) {
        return stderr == null || stderr.trim().length() == 0;
    }

    private static boolean stderrIndicatesLaunchRejected(final String stderr) {
        if (stderr == null) {
            return false;
        }
        final String s = stderr.toLowerCase();
        return s.indexOf("win32exception") >= 0 || s.indexOf("zugriff verweigert") >= 0 || s.indexOf("access denied") >= 0 || s.indexOf("privilege") >= 0 || s.indexOf(" 1314") >= 0 || s.indexOf("runashelper") >= 0 || s.indexOf("runtasprocesslauncher") >= 0 || s.indexOf("createenvironmentblock") >= 0;
    }

    private static boolean stdoutContainsSid(final IdentityProbeRun run, final String sid) {
        if (run == null || sid == null || run.stdout == null) {
            return false;
        }
        return run.stdout.toLowerCase().indexOf(sid.toLowerCase()) >= 0;
    }

    private static String[] powershellIdentityArgv() {
        return new String[] { "powershell.exe", "-NoProfile", "-NonInteractive", "-Command", PS_PROBE_SESSION_SID_ELEV };
    }

    private static String[] whoamiArgv() {
        return new String[] { "cmd.exe", "/c", "whoami", "/user" };
    }

    private static String firstLineOfStdout(final String stdout) {
        if (stdout == null || stdout.trim().length() == 0) {
            return "";
        }
        return stdout.split("\r?\n", -1)[0].trim();
    }

    private static IdentityProbeRun captureProbeRun(final ProcessOutput out) {
        final IdentityProbeRun run = new IdentityProbeRun();
        if (out == null) {
            run.exitCode = -1;
            run.stdout = "";
            run.stderr = "null ProcessOutput";
            run.firstLine = "";
            return run;
        }
        run.exitCode = out.getExitCode();
        run.stdout = out.getStdOutString() != null ? out.getStdOutString() : "";
        run.stderr = out.getErrOutString() != null ? out.getErrOutString() : "";
        run.firstLine = firstLineOfStdout(run.stdout);
        return run;
    }

    private static IdentityProbeRun captureProbeRunFromThrowable(final Throwable t) {
        final IdentityProbeRun run = new IdentityProbeRun();
        run.exitCode = -1;
        run.stdout = "";
        run.stderr = Exceptions.getStackTrace(t);
        run.firstLine = "";
        return run;
    }

    private static ParsedIdentityLine parseIdentityProbeOutput(final String label, final ProcessOutput out) throws Exception {
        final IdentityProbeRun run = captureProbeRun(out);
        if (run.exitCode != 0) {
            // Probe process must exit 0 — otherwise stdout is not a valid identity line.
            throw new Exception(label + ": identity probe exit " + run.exitCode + ", stderr=" + run.stderr);
        }
        if (!stderrBlank(run.stderr)) {
            // Any stderr fails the probe — identity parsing must not proceed on noisy output.
            throw new Exception(label + ": identity probe stderr=" + run.stderr);
        }
        return parseIdentityFirstLine(label, run.firstLine);
    }

    private static ParsedIdentityLine parseIdentityFirstLine(final String label, final String firstLine) throws Exception {
        if (firstLine == null || firstLine.trim().length() == 0) {
            // Empty first line cannot be session|sid|elev — treat as hard identity failure.
            throw new Exception(label + ": empty identity first line");
        }
        final String[] parts = firstLine.trim().split("\\|", -1);
        if (parts.length != 3) {
            // Exactly three pipe-separated fields are required for a machine-readable identity line.
            throw new Exception(label + ": expected session|sid|elev, got: " + firstLine);
        }
        final int session;
        try {
            session = Integer.parseInt(parts[0].trim());
        } catch (NumberFormatException e) {
            // Session field must be an integer WTS session id.
            throw new Exception(label + ": bad session: " + parts[0], e);
        }
        final String sid = parts[1].trim().toLowerCase();
        final String elev = parts[2].trim();
        if (!"0".equals(elev) && !"1".equals(elev)) {
            // elev is a boolean flag encoded as 0/1 only — reject other tokens.
            throw new Exception(label + ": elev must be 0 or 1, got: " + elev);
        }
        final String machineLine = session + "|" + sid + "|" + elev;
        return new ParsedIdentityLine(session, sid, elev, machineLine);
    }

    private static void fillLaunchingJvmIdentity(final IdentityJvmSnapshot snapshot) throws Exception {
        snapshot.windowsUtilsElevated = WindowsUtils.isElevated();
        snapshot.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
        snapshot.launchingJvmIdentityProbe = captureProbeRun(ProcessBuilderFactory.runCommand(powershellIdentityArgv()));
        snapshot.launchingJvmIdentityParsed = parseIdentityFirstLine("launchingJvm", snapshot.launchingJvmIdentityProbe.firstLine);
        snapshot.launchingJvmIdentity = snapshot.launchingJvmIdentityParsed;
        snapshot.launchingJvmWhoamiProbe = captureProbeRun(ProcessBuilderFactory.runCommand(whoamiArgv()));
    }

    private static void launchSessionOwnerIdentityProbe(final IdentityJvmSnapshot snapshot, final OwnerLaunchMode mode, final int interactiveSessionId) {
        try {
            final ProcessOutput identityOut;
            final ProcessOutput whoamiOut;
            if (mode == OwnerLaunchMode.INTERACTIVE_SESSION) {
                identityOut = RunAsHelper.runInSession(interactiveSessionId, powershellIdentityArgv(), RunAsLaunchOptions.DEFAULT);
                whoamiOut = RunAsHelper.runInSession(interactiveSessionId, whoamiArgv(), RunAsLaunchOptions.DEFAULT);
            } else {
                identityOut = RunAsHelper.runInOwnerSession(powershellIdentityArgv(), RunAsLaunchOptions.DEFAULT);
                whoamiOut = RunAsHelper.runInOwnerSession(whoamiArgv(), RunAsLaunchOptions.DEFAULT);
            }
            snapshot.sessionOwnerIdentityProbe = captureProbeRun(identityOut);
            if (snapshot.sessionOwnerIdentityProbe.exitCode == 0 && stderrBlank(snapshot.sessionOwnerIdentityProbe.stderr)) {
                snapshot.sessionOwnerIdentityParsed = parseIdentityFirstLine("sessionOwner", snapshot.sessionOwnerIdentityProbe.firstLine);
                snapshot.sessionOwnerIdentity = snapshot.sessionOwnerIdentityParsed;
            }
            snapshot.sessionOwnerWhoamiProbe = captureProbeRun(whoamiOut);
        } catch (Throwable t) {
            snapshot.sessionOwnerIdentityProbe = captureProbeRunFromThrowable(t);
            snapshot.sessionOwnerWhoamiProbe = captureProbeRunFromThrowable(t);
        }
    }

    private enum OwnerLaunchMode {
        CURRENT_SESSION,
        INTERACTIVE_SESSION
    }

    public static final class InteractiveOwnerBaseline implements Serializable {
        private static final long serialVersionUID = 1L;
        public int                interactiveSessionId;
        public String             ownerSid;
        public String             ownerElev;
        public String             machineLine;

        public InteractiveOwnerBaseline() {
        }
    }

    public static final class ParsedIdentityLine implements Serializable {
        private static final long serialVersionUID = 1L;
        public int                session;
        public String             sid;
        public String             elev;
        public String             machineLine;

        public ParsedIdentityLine() {
        }

        ParsedIdentityLine(final int session, final String sid, final String elev, final String machineLine) {
            this.session = session;
            this.sid = sid != null ? sid : "";
            this.elev = elev != null ? elev : "";
            this.machineLine = machineLine != null ? machineLine : "";
        }
    }

    public static final class IdentityProbeRun implements Serializable {
        private static final long serialVersionUID = 1L;
        public int                exitCode;
        public String             stdout;
        public String             stderr;
        public String             firstLine;

        public IdentityProbeRun() {
        }
    }

    public static final class IdentityJvmSnapshot implements Serializable {
        private static final long serialVersionUID       = 1L;
        public boolean            windowsUtilsElevated;
        public boolean            isLocalSystem;
        /**
         * Set by LocalSystem runInOwnerSession scenario: result of
         * {@link InteractiveSessionOwner#resolveOwnerSessionIdForCurrentProcess()}.
         */
        public int                resolvedOwnerSessionId = -1;
        public ParsedIdentityLine launchingJvmIdentity;
        public IdentityProbeRun   launchingJvmIdentityProbe;
        public ParsedIdentityLine launchingJvmIdentityParsed;
        public IdentityProbeRun   launchingJvmWhoamiProbe;
        public ParsedIdentityLine sessionOwnerIdentity;
        public IdentityProbeRun   sessionOwnerIdentityProbe;
        public ParsedIdentityLine sessionOwnerIdentityParsed;
        public IdentityProbeRun   sessionOwnerWhoamiProbe;
        public boolean            hasTrayWindow;

        public IdentityJvmSnapshot() {
        }
    }

    public static final class NestedIdentityReport implements Serializable {
        private static final long  serialVersionUID = 1L;
        public IdentityJvmSnapshot adminCapableTestUserSnapshot;
        public IdentityJvmSnapshot elevatedAdminSnapshot;
        public String              nestedFailureStack;

        public NestedIdentityReport() {
        }
    }

    private static final class LocalSystemToSessionOwnerIdentityTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final int         interactiveSessionId;

        LocalSystemToSessionOwnerIdentityTask(final int interactiveSessionId) {
            this.interactiveSessionId = interactiveSessionId;
        }

        @Override
        public Serializable run() throws Exception {
            if (!WindowsUtils.isRunningAsLocalSystem()) {
                // Task must run as LocalSystem — otherwise scenario 01 is not testing the SYSTEM→owner path.
                throw new Exception("LocalSystemToSessionOwnerIdentityTask requires SYSTEM");
            }
            final IdentityJvmSnapshot snapshot = new IdentityJvmSnapshot();
            fillLaunchingJvmIdentity(snapshot);
            // Step 2: Launch session-owner identity probe (runInSession from Local System)
            LogV3.info("RunAsHelperIdentityTest 01 LocalSystem: pre-token diagnostics for session " + interactiveSessionId);
            SessionUserTokens.logTokenResolutionDiagnostics(interactiveSessionId);
            launchSessionOwnerIdentityProbe(snapshot, OwnerLaunchMode.INTERACTIVE_SESSION, interactiveSessionId);
            HWND hwnd = User32.INSTANCE.FindWindow("Shell_TrayWnd", null);
            int gleFw = Kernel32.INSTANCE.GetLastError();
            RunAsWin32ApiTrace.out("SessionUserTokens", "FindWindow", hwnd != null, gleFw);
            snapshot.hasTrayWindow = hwnd != null;
            LogV3.info("RunAsHelperIdentityTest 01 LocalSystem: FindWindow(Shell_TrayWnd) from SYSTEM session0 visible=" + snapshot.hasTrayWindow + " gle=" + gleFw);
            return snapshot;
        }
    }

    /**
     * Installer-style path: LocalSystem in session 0 must reach the interactive owner via {@link RunAsHelper#runInOwnerSession}
     * (console-session redirect), not only via explicit {@link RunAsHelper#runInSession}.
     */
    private static final class LocalSystemToSessionOwnerViaRunInOwnerSessionIdentityTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final int         expectedInteractiveSessionId;

        LocalSystemToSessionOwnerViaRunInOwnerSessionIdentityTask(final int expectedInteractiveSessionId) {
            this.expectedInteractiveSessionId = expectedInteractiveSessionId;
        }

        @Override
        public Serializable run() throws Exception {
            if (!WindowsUtils.isRunningAsLocalSystem()) {
                throw new Exception("LocalSystemToSessionOwnerViaRunInOwnerSessionIdentityTask requires SYSTEM");
            }
            final IdentityJvmSnapshot snapshot = new IdentityJvmSnapshot();
            fillLaunchingJvmIdentity(snapshot);
            // Tried openForCurrentProcess(processSession=0) alone: fails (no Shell_TrayWnd in session 0). Redirect via Explorer/WTSActive.
            snapshot.resolvedOwnerSessionId = InteractiveSessionOwner.resolveOwnerSessionIdForCurrentProcess();
            if (snapshot.resolvedOwnerSessionId != expectedInteractiveSessionId) {
                throw new Exception("resolveOwnerSessionIdForCurrentProcess expected " + expectedInteractiveSessionId + " but got " + snapshot.resolvedOwnerSessionId + " (processSession=" + WindowsUtils.getCurrentProcessSessionId() + ")");
            }
            LogV3.info("RunAsHelperIdentityTest 01b LocalSystem: resolvedOwnerSessionId=" + snapshot.resolvedOwnerSessionId + " processSession=" + WindowsUtils.getCurrentProcessSessionId());
            SessionUserTokens.logTokenResolutionDiagnostics(snapshot.resolvedOwnerSessionId);
            launchSessionOwnerIdentityProbe(snapshot, OwnerLaunchMode.CURRENT_SESSION, -1);
            return snapshot;
        }
    }

    private static final class ElevatedAdminToSessionOwnerIdentityTask implements ElevatedTestTask {
        private static final long     serialVersionUID = 1L;
        private final OwnerLaunchMode launchMode;
        private final int             interactiveSessionId;

        ElevatedAdminToSessionOwnerIdentityTask(final OwnerLaunchMode launchMode, final int interactiveSessionId) {
            this.launchMode = launchMode != null ? launchMode : OwnerLaunchMode.CURRENT_SESSION;
            this.interactiveSessionId = interactiveSessionId;
        }

        @Override
        public Serializable run() throws Exception {
            final IdentityJvmSnapshot snapshot = new IdentityJvmSnapshot();
            fillLaunchingJvmIdentity(snapshot);
            // Step 2: Launch session-owner identity probe (runInOwnerSession from elevated admin)
            launchSessionOwnerIdentityProbe(snapshot, launchMode, interactiveSessionId);
            return snapshot;
        }
    }

    private static final class PlainTestUserToSessionOwnerIdentityTask implements ElevatedTestTask {
        private static final long     serialVersionUID = 1L;
        private final OwnerLaunchMode launchMode;

        PlainTestUserToSessionOwnerIdentityTask(final OwnerLaunchMode launchMode) {
            this.launchMode = launchMode != null ? launchMode : OwnerLaunchMode.CURRENT_SESSION;
        }

        @Override
        public Serializable run() throws Exception {
            final IdentityJvmSnapshot snapshot = new IdentityJvmSnapshot();
            fillLaunchingJvmIdentity(snapshot);
            // Step 2: runInOwnerSession must FAIL for plain non-admin user (privilege)
            launchSessionOwnerIdentityProbe(snapshot, launchMode, -1);
            return snapshot;
        }
    }

    private static final class AdminCapableTestUserToSessionOwnerIdentityTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final NestedIdentityReport report = new NestedIdentityReport();
            report.adminCapableTestUserSnapshot = new IdentityJvmSnapshot();
            fillLaunchingJvmIdentity(report.adminCapableTestUserSnapshot);
            // Step 2: runInOwnerSession from admin-capable but non-elevated runAsUser JVM must FAIL (mirror Cwd scenario 04)
            launchSessionOwnerIdentityProbe(report.adminCapableTestUserSnapshot, OwnerLaunchMode.CURRENT_SESSION, -1);
            try {
                // Step 3: Nested elevated-admin task; session-owner probe must match InteractiveOwnerBaseline
                report.elevatedAdminSnapshot = AdminExecuter.runAsAdmin(new ElevatedAdminToSessionOwnerIdentityTask(OwnerLaunchMode.CURRENT_SESSION, -1), TYPE_IDENTITY_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
            } catch (Throwable t) {
                report.nestedFailureStack = Exceptions.getStackTrace(t);
            }
            return report;
        }
    }

    private static final class CreateTestWindowsUserTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      userName;
        private final String      password;
        private final boolean     addToAdministrators;

        CreateTestWindowsUserTask(final String userName, final String password, final boolean addToAdministrators) {
            this.userName = userName != null ? userName : "";
            this.password = password != null ? password : "";
            this.addToAdministrators = addToAdministrators;
        }

        @Override
        public Serializable run() throws Exception {
            final ProcessOutput existsProbe = ProcessBuilderFactory.runCommand("net", "user", userName);
            if (existsProbe.getExitCode() == 0) {
                ProcessBuilderFactory.runCommand("net", "user", userName, "/delete");
            }
            final ProcessOutput add = ProcessBuilderFactory.runCommand("net", "user", userName, password, "/add", "/expires:never", "/passwordchg:no");
            if (add.getExitCode() != 0) {
                // Test user must be created — later scenarios depend on this account existing.
                throw new Exception("net user add failed: " + add.getErrOutString());
            }
            if (addToAdministrators) {
                final Advapi32Util.Account adminGroup = Advapi32Util.getAccountBySid(WindowsUtils.SID.SID_BUILTIN_ADMINISTRATORS.sid);
                final ProcessOutput groupAdd = ProcessBuilderFactory.runCommand("net", "localgroup", adminGroup.name, userName, "/add");
                if (groupAdd.getExitCode() != 0) {
                    // Admin-capable scenarios require Administrators membership for nested elevation.
                    throw new Exception("net localgroup add failed: " + groupAdd.getErrOutString());
                }
            }
            return Boolean.TRUE;
        }
    }

    private static final class DeleteTestWindowsUserTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      userName;

        DeleteTestWindowsUserTask(final String userName) {
            this.userName = userName != null ? userName : "";
        }

        @Override
        public Serializable run() throws Exception {
            ProcessBuilderFactory.runCommand("net", "user", userName, "/delete");
            return null;
        }
    }

    private void deleteTestUserQuietly(final String userName) {
        try {
            AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        } catch (Throwable t) {
            logInfoAnyway("RunAsHelperIdentityTest: cleanup failed (" + userName + "): " + t.getMessage());
        }
    }
}
