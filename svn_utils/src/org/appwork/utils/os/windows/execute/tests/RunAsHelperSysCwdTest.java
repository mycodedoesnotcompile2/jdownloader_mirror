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
package org.appwork.utils.os.windows.execute.tests;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.EnumSet;
import java.util.Set;

import org.appwork.JNAHelper;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestTag;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.InteractiveSessionOwner;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.KnownFolders;
import com.sun.jna.platform.win32.ShlObj;

/**
 * WorkingDir rules when launching as the interactive session owner.
 * <ul>
 * <li>LocalSystem → {@link RunAsHelper#runInSession}(explicit session id)</li>
 * <li>Elevated admin → {@link RunAsHelper#runInOwnerSession} (owner of the elevated process session)</li>
 * </ul>
 * Shared denied/allowed workingDir setup:
 * <ul>
 * <li><strong>Denied dir</strong> — temp folder; ACL denies the session-owner SID (launching JVM may still open it).</li>
 * <li><strong>Allowed dir</strong> — under the session-owner profile; session-owner launch must use that cwd.</li>
 * </ul>
 * Privilege-rejection scenarios (plain / unelevated admin-capable user) also use {@link RunAsHelper#runInOwnerSession}.
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperSysCwdTest extends AWTest {
    private static final String                               PS_PROBE_CWD                         = "Write-Output ((Get-Location).Path)";
    private static final String                               USER_PLAIN                           = "RunAsEnvPlainUser";
    private static final String                               USER_ADMIN_CAPABLE                   = "RunAsEnvTestUser";
    private static final TypeRef<CwdProbeJvmSnapshot>         TYPE_CWD_PROBE_JVM_SNAPSHOT          = new TypeRef<CwdProbeJvmSnapshot>() {
                                                                                                   };
    private static final TypeRef<NestedSessionOwnerCwdReport> TYPE_NESTED_SESSION_OWNER_CWD_REPORT = new TypeRef<NestedSessionOwnerCwdReport>() {
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
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("RunAsHelperSysCwdTest: Windows only, skipped.");
            return;
        }
        if (!JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperSysCwdTest: JNA not available, skipped.");
            return;
        }
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("RunAsHelperSysCwdTest: skip all scenarios (no interactive WTS session id).");
            return;
        }
        testLocalSystemTriesToRunAProcessAsSessionOwnerInForbiddenWorkingDir(interactiveSessionId);
        testElevatedAdminTriesToRunAProcessAsSessionOwnerInForbiddenWorkingDir();
        testPlainNonAdminUserCannotRunInOwnerSession();
        testAdminCapableUnelevatedUserCannotRunInOwnerSession_ThenElevatedAdminAppliesWorkingDirRules();
    }

    /**
     * LocalSystem calls {@link RunAsHelper#runInSession}: denied workingDir must fail for the session owner; allowed profile dir must
     * succeed with matching cwd. Launching JVM sanity probe must still open the denied dir (ACL targets session owner only).
     */
    private void testLocalSystemTriesToRunAProcessAsSessionOwnerInForbiddenWorkingDir(final int interactiveSessionId) throws Exception {
        final String marker = "LOCAL_SYSTEM_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        final CwdProbeJvmSnapshot snapshot = AdminExecuter.runAsLocalSystem(new LocalSystemSessionOwnerWorkingDirTask(interactiveSessionId, marker), TYPE_CWD_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        assertLocalSystemSessionOwnerWorkingDirRules(snapshot);
    }

    /**
     * Same denied/allowed workingDir rules as LocalSystem, but launching JVM is elevated admin and uses
     * {@link RunAsHelper#runInOwnerSession} (owner of the elevated process's own session — not {@link RunAsHelper#runInSession}, which
     * typically needs LocalSystem/SeTcbPrivilege for {@code WTSQueryUserToken}).
     */
    private void testElevatedAdminTriesToRunAProcessAsSessionOwnerInForbiddenWorkingDir() throws Exception {
        final String marker = "ELEVATED_ADMIN_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        final CwdProbeJvmSnapshot snapshot = AdminExecuter.runAsAdmin(new ElevatedAdminSessionOwnerWorkingDirTask(marker), TYPE_CWD_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        assertElevatedAdminSessionOwnerWorkingDirRules(snapshot);
    }

    /**
     * Plain non-admin user: {@link RunAsHelper#runInOwnerSession} must fail for privilege reasons (no icacls deny needed).
     */
    private void testPlainNonAdminUserCannotRunInOwnerSession() throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_PLAIN, password, false), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final String marker = "PLAIN_TEST_USER_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
            final CwdProbeJvmSnapshot snapshot = AdminExecuter.runAsUser(null, USER_PLAIN, password, new PlainUserCannotRunInOwnerSessionTask(marker), TYPE_CWD_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
            // Plain-user task must return a snapshot so we can check privilege rejection.
            assertTrue(snapshot != null, "plain-user: snapshot is null");
            // Task JVM must actually run as the created plain test account (not the interactive/session owner).
            assertTrue(snapshot.launchingJvmUserName != null && snapshot.launchingJvmUserName.length() > 0, "plain-user: launchingJvmUserName missing");
            assertTrue(userNameMatchesAccount(snapshot.launchingJvmUserName, USER_PLAIN), "plain-user: launchingJvmUserName expected=" + USER_PLAIN + ", got=" + snapshot.launchingJvmUserName);
            // Plain non-admin user must not be elevated — scenario covers privilege-only failure.
            assertFalse(snapshot.isElevated, "plain-user: must not be elevated");
            // Launching JVM must still use the temp workingDir (sanity that the folder itself is fine).
            assertProbeRunProcessOk("plain-user/launchingJvmSanity", snapshot.launchingJvmSanityDeniedProbe);
            // runInOwnerSession from plain user must fail — lacks privileges, even without icacls deny.
            assertSessionOwnerLaunchRejected("plain-user/runInOwnerSession", snapshot.sessionOwnerDeniedWorkingDirProbe);
        } finally {
            deleteTestUserQuietly(USER_PLAIN);
        }
    }

    /**
     * Admin-capable but filtered (not elevated) user: {@link RunAsHelper#runInOwnerSession} must fail. Nested elevated admin then applies
     * the same denied/allowed workingDir rules as {@link #testElevatedAdminTriesToRunAProcessAsSessionOwnerInForbiddenWorkingDir()}.
     */
    private void testAdminCapableUnelevatedUserCannotRunInOwnerSession_ThenElevatedAdminAppliesWorkingDirRules() throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_ADMIN_CAPABLE, password, true), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final String marker = "ADMIN_CAPABLE_TEST_USER_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
            final NestedSessionOwnerCwdReport report = AdminExecuter.runAsUser(null, USER_ADMIN_CAPABLE, password, new AdminCapableUnelevatedThenElevatedWorkingDirTask(marker), TYPE_NESTED_SESSION_OWNER_CWD_REPORT, ProcessOptions.DEFAULT);
            // Nested report must exist — outer runAsUser task returns both rejection and elevated probes.
            assertTrue(report != null, "admin-capable: report is null");
            // Non-elevated admin-capable snapshot is required for the privilege-rejection check.
            assertTrue(report.adminCapableTestUserSnapshot != null, "admin-capable: unelevated snapshot missing");
            // runAsUser JVM must stay filtered/non-elevated — elevation would change privilege expectations.
            assertFalse(report.adminCapableTestUserSnapshot.isElevated, "admin-capable: runAsUser must not be elevated");
            // From non-elevated admin-capable user, session-owner launch must fail (privilege).
            assertSessionOwnerLaunchRejected("admin-capable/unelevated/runInOwnerSession", report.adminCapableTestUserSnapshot.sessionOwnerDeniedWorkingDirProbe);
            // Nested elevated-admin task must have returned a cwd snapshot.
            assertTrue(report.elevatedAdminSnapshot != null, "admin-capable: elevated snapshot missing, nested:\n" + report.nestedFailureStack);
            // Nested elevated run must not have thrown — otherwise cwd rules were not exercised.
            assertTrue(report.nestedFailureStack == null || report.nestedFailureStack.trim().length() == 0, "admin-capable: nested failed:\n" + report.nestedFailureStack);
            // Nested elevated admin must obey the same denied/allowed workingDir rules.
            assertElevatedAdminSessionOwnerWorkingDirRules(report.elevatedAdminSnapshot);
        } finally {
            deleteTestUserQuietly(USER_ADMIN_CAPABLE);
        }
    }

    private void deleteTestUserQuietly(final String userName) {
        try {
            AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        } catch (Throwable t) {
            logInfoAnyway("RunAsHelperSysCwdTest: cleanup net user delete failed (" + userName + "): " + t.getMessage());
        }
    }

    private void assertLocalSystemSessionOwnerWorkingDirRules(final CwdProbeJvmSnapshot snapshot) throws Exception {
        final String label = "local-system->sessionOwner";
        assertTrue(snapshot != null, label + ": snapshot is null");
        assertTrue(snapshot.isLocalSystem, label + ": launching JVM must be Local System");
        assertDeniedAndAllowedWorkingDirProbes(label, snapshot);
    }

    private void assertElevatedAdminSessionOwnerWorkingDirRules(final CwdProbeJvmSnapshot snapshot) throws Exception {
        final String label = "elevated-admin->sessionOwner";
        assertTrue(snapshot != null, label + ": snapshot is null");
        assertEquals(true, snapshot.isElevated, label + ": launching JVM must be elevated");
        assertDeniedAndAllowedWorkingDirProbes(label, snapshot);
    }

    private void assertDeniedAndAllowedWorkingDirProbes(final String label, final CwdProbeJvmSnapshot snapshot) throws Exception {
        // Denied dir path is required so we can verify session-owner rejection against that ACL.
        assertTrue(snapshot.deniedWorkingDirPath != null && snapshot.deniedWorkingDirPath.length() > 0, label + ": deniedWorkingDirPath missing");
        // Allowed dir under session-owner profile is required as the expected successful cwd.
        assertTrue(snapshot.sessionOwnerAllowedWorkingDirPath != null && snapshot.sessionOwnerAllowedWorkingDirPath.length() > 0, label + ": sessionOwnerAllowedWorkingDirPath missing");
        // Launching JVM must still open the denied dir — ACL targets session owner, not the helper JVM.
        // (We do not assert path-string equality here: File#getAbsolutePath vs PowerShell Get-Location can differ in 8.3 form.)
        assertProbeRunProcessOk(label + "/launchingJvmSanityDenied", snapshot.launchingJvmSanityDeniedProbe);
        // Session-owner token must not access the denied workingDir — launch must fail.
        assertSessionOwnerLaunchRejected(label + "/sessionOwnerDenied", snapshot.sessionOwnerDeniedWorkingDirProbe);
        // Session-owner launch with allowed workingDir must succeed (exit 0, clean stderr).
        assertProbeRunProcessOk(label + "/sessionOwnerAllowed", snapshot.sessionOwnerAllowedWorkingDirProbe);
        // Child cwd must equal the allowed path we set — session-owner token has access only there.
        assertCwdProbeMatchesPath(label + "/sessionOwnerAllowed", snapshot.sessionOwnerAllowedWorkingDirPath, snapshot.sessionOwnerAllowedWorkingDirProbe);
    }

    private void assertSessionOwnerLaunchRejected(final String label, final CwdProbeRun sessionOwnerRun) throws Exception {
        // Probe capture must exist (either ProcessOutput or exception stack as stderr).
        assertTrue(sessionOwnerRun != null, label + ": session owner probe run is null");
        // Session-owner launch must fail — denied path or missing privilege.
        assertTrue(sessionOwnerRun.exitCode != 0, label + ": session owner launch must fail, exit=" + sessionOwnerRun.exitCode);
        // Failure must leave stderr so we can classify access/privilege rejection.
        assertTrue(!stderrBlank(sessionOwnerRun.stderr), label + ": expected exception stderr");
        // Stderr must indicate access denied / privilege / workingDir not accessible (not an unrelated error).
        assertTrue(stderrIndicatesSessionOwnerLaunchRejected(sessionOwnerRun.stderr), label + ": stderr=" + sessionOwnerRun.stderr);
    }

    private void assertCwdProbeMatchesPath(final String label, final String expectedPath, final CwdProbeRun run) throws Exception {
        final String probePath = run != null ? run.firstLine : null;
        // Probe first line (Get-Location) must equal the workingDir we configured, after Windows path normalize.
        assertTrue(winPathsEqual(expectedPath, probePath), label + ": cwd, expected=" + expectedPath + ", probe=" + probePath + ", stdout=" + (run != null ? run.stdout : null));
    }

    static boolean winPathsEqual(final String a, final String b) {
        if (a == null || b == null) {
            return false;
        }
        final String na = normalizeWinPath(a);
        final String nb = normalizeWinPath(b);
        return na.length() > 0 && na.equals(nb);
    }

    /** Match short account name against {@code DOMAIN\\user}, {@code user@domain}, or bare {@code user}. */
    static boolean userNameMatchesAccount(final String actual, final String expectedAccount) {
        if (actual == null || expectedAccount == null) {
            return false;
        }
        final String a = actual.trim();
        final String e = expectedAccount.trim();
        if (a.length() == 0 || e.length() == 0) {
            return false;
        }
        if (a.equalsIgnoreCase(e)) {
            return true;
        }
        final int slash = a.lastIndexOf('\\');
        if (slash >= 0 && a.substring(slash + 1).equalsIgnoreCase(e)) {
            return true;
        }
        final int at = a.indexOf('@');
        if (at > 0 && a.substring(0, at).equalsIgnoreCase(e)) {
            return true;
        }
        return false;
    }

    private void assertProbeRunProcessOk(final String label, final CwdProbeRun run) throws Exception {
        // Probe must have been captured — missing run means launch never ran.
        assertTrue(run != null, label + ": probe run is null");
        // Successful cwd probe must exit 0 (powershell Get-Location).
        assertEquals(0, run.exitCode, label + ": exitCode, stderr=" + run.stderr + ", stdout=" + run.stdout);
        // Clean success: no stderr noise that would hide a partial failure.
        assertTrue(stderrBlank(run.stderr), label + ": stderr not empty: " + run.stderr);
        // Need a non-empty first line — the cwd path we compare against expected.
        assertTrue(run.firstLine != null && run.firstLine.length() > 0, label + ": firstLine missing, stdout=" + run.stdout);
    }

    private static boolean stderrBlank(final String stderr) {
        return stderr == null || stderr.trim().length() == 0;
    }

    private static boolean stderrIndicatesSessionOwnerLaunchRejected(final String stderr) {
        if (stderr == null) {
            return false;
        }
        final String s = stderr.toLowerCase();
        return s.indexOf("win32exception") >= 0 || s.indexOf("zugriff verweigert") >= 0 || s.indexOf("access denied") >= 0 || s.indexOf("privilege") >= 0 || s.indexOf(" 1314") >= 0 || s.indexOf("createenvironmentblock") >= 0 || s.indexOf("createprocesswithtoken") >= 0 || s.indexOf("createprocessasuser") >= 0 || s.indexOf("runtasprocesslauncher") >= 0 || s.indexOf("runashelper") >= 0 || s.indexOf("workingdir not accessible") >= 0 || s.indexOf("not accessible for target user") >= 0;
    }

    static String normalizeWinPath(final String path) {
        if (path == null) {
            return "";
        }
        String p = path.trim().replace('/', '\\');
        while (p.length() > 3 && p.endsWith("\\")) {
            p = p.substring(0, p.length() - 1);
        }
        if (p.length() > 0) {
            p = expandWinPathForCompare(p);
        }
        return p.toLowerCase();
    }

    static String expandWinPathForCompare(String p) {
        try {
            final File file = new File(p);
            if (file.exists()) {
                return file.getCanonicalPath();
            }
        } catch (IOException e) {
            // keep p
        }
        return p;
    }

    private static String[] powershellCwdProbeArgv() {
        return new String[] { "powershell.exe", "-NoProfile", "-NonInteractive", "-Command", PS_PROBE_CWD };
    }

    private static RunAsLaunchOptions launchOptionsWithWorkingDir(final File workingDir) {
        return RunAsLaunchOptions.builder().waitFor(true).workingDir(workingDir).build();
    }

    private static String firstLineOfStdout(final String stdout) {
        if (stdout == null || stdout.trim().length() == 0) {
            return "";
        }
        return stdout.split("\r?\n", -1)[0].trim();
    }

    private static CwdProbeRun captureProbeRun(final ProcessOutput out) {
        final CwdProbeRun run = new CwdProbeRun();
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

    private static CwdProbeRun captureProbeRunFromThrowable(final Throwable t) {
        final CwdProbeRun run = new CwdProbeRun();
        run.exitCode = -1;
        run.stdout = "";
        run.stderr = Exceptions.getStackTrace(t);
        run.firstLine = "";
        return run;
    }

    /** Try session-owner launch; return probe from ProcessOutput or from thrown exception. */
    private static CwdProbeRun tryCapture(final CallableLaunch launch) {
        try {
            return captureProbeRun(launch.run());
        } catch (Throwable t) {
            return captureProbeRunFromThrowable(t);
        }
    }

    private interface CallableLaunch {
        ProcessOutput run() throws Exception;
    }

    private static File createLaunchingJvmWorkingDir(final String markerSegment) throws Exception {
        final String tempRoot = System.getenv("TEMP");
        final String systemRoot = System.getenv("SystemRoot");
        File parent;
        if (tempRoot != null && tempRoot.trim().length() > 0) {
            parent = new File(tempRoot.trim());
        } else if (systemRoot != null && systemRoot.trim().length() > 0) {
            parent = new File(systemRoot.trim(), "Temp");
        } else {
            parent = new File("C:\\Windows\\Temp");
        }
        final File dir = new File(parent, "APWORK_RUNAS_CWD_" + markerSegment);
        if (!dir.mkdirs() && !dir.isDirectory()) {
            throw new Exception("createLaunchingJvmWorkingDir failed: " + dir.getAbsolutePath());
        }
        return dir;
    }

    private static File createSessionOwnerAllowedWorkingDir(final int interactiveSessionId, final String markerSegment) throws Exception {
        final int sessionId = interactiveSessionId >= 0 ? interactiveSessionId : WindowsUtils.getCurrentProcessSessionId();
        if (sessionId < 0) {
            throw new Exception("createSessionOwnerAllowedWorkingDir: no WTS session id");
        }
        final InteractiveSessionOwner sessionOwner = InteractiveSessionOwner.openForSession(sessionId);
        try {
            final String profile = RunAsHelper.getKnownFolderPath(sessionOwner.getUserTokenHandle(), KnownFolders.FOLDERID_Profile, ShlObj.KNOWN_FOLDER_FLAG.NONE.getFlag());
            final File dir = new File(profile, "APWORK_RUNAS_CWD_OK_" + markerSegment);
            if (!dir.mkdirs() && !dir.isDirectory()) {
                throw new Exception("createSessionOwnerAllowedWorkingDir failed: " + dir.getAbsolutePath());
            }
            return dir;
        } finally {
            sessionOwner.close();
        }
    }

    /**
     * Deny the interactive session-owner SID on {@code dir}. Grants SYSTEM full control so Local System can still create and probe the
     * folder.
     */
    private static void restrictDirectoryDenySessionOwner(final File dir, final int interactiveSessionId) throws Exception {
        final int sessionId = interactiveSessionId >= 0 ? interactiveSessionId : WindowsUtils.getCurrentProcessSessionId();
        if (sessionId < 0) {
            throw new Exception("restrictDirectoryDenySessionOwner: no WTS session id");
        }
        final InteractiveSessionOwner sessionOwner = InteractiveSessionOwner.openForSession(sessionId);
        try {
            restrictDirectoryDenySid(dir, sessionOwner.getOwnerSid());
        } finally {
            sessionOwner.close();
        }
    }

    private static void restrictDirectoryDenySid(final File dir, final String sessionOwnerSid) throws Exception {
        final String path = dir.getAbsolutePath();
        final ProcessOutput grantSystem = ProcessBuilderFactory.runCommand("icacls", path, "/inheritance:r", "/grant:r", "SYSTEM:(OI)(CI)F");
        if (grantSystem.getExitCode() != 0) {
            throw new Exception("icacls grant SYSTEM failed: " + grantSystem.getErrOutString());
        }
        if (sessionOwnerSid != null && sessionOwnerSid.trim().length() > 0) {
            final ProcessOutput denySessionOwner = ProcessBuilderFactory.runCommand("icacls", path, "/deny", "*" + sessionOwnerSid.trim() + ":(OI)(CI)F");
            if (denySessionOwner.getExitCode() != 0) {
                throw new Exception("icacls deny session owner failed: " + denySessionOwner.getErrOutString());
            }
        }
    }

    /**
     * Best-effort delete of test working dirs. Restores access first because {@link #restrictDirectoryDenySid} may leave SYSTEM-only ACLs
     * that block the current (elevated) user from deleting.
     */
    private static void deleteTestWorkingDirQuietly(final File dir) {
        if (dir == null) {
            return;
        }
        try {
            if (!dir.exists()) {
                return;
            }
        } catch (Throwable t) {
            return;
        }
        final String path = dir.getAbsolutePath();
        try {
            ProcessBuilderFactory.runCommand("takeown", "/F", path, "/R", "/D", "Y");
        } catch (Throwable ignore) {
        }
        try {
            ProcessBuilderFactory.runCommand("icacls", path, "/grant", "Administrators:(OI)(CI)F", "/T", "/C", "/Q");
        } catch (Throwable ignore) {
        }
        try {
            ProcessBuilderFactory.runCommand("icacls", path, "/reset", "/T", "/C", "/Q");
        } catch (Throwable ignore) {
        }
        try {
            if (dir.exists()) {
                Files.deleteRecursive(dir, false);
            }
        } catch (Throwable ignore) {
        }
    }

    public static final class CwdProbeRun implements Serializable {
        private static final long serialVersionUID = 1L;
        public int                exitCode;
        public String             stdout;
        public String             stderr;
        public String             firstLine;

        public CwdProbeRun() {
        }
    }

    public static final class CwdProbeJvmSnapshot implements Serializable {
        private static final long serialVersionUID = 1L;
        public boolean            isElevated;
        public boolean            isLocalSystem;
        /** Windows account name of the launching JVM (e.g. from {@link Advapi32Util#getUserName()}). */
        public String             launchingJvmUserName;
        public String             deniedWorkingDirPath;
        public String             sessionOwnerAllowedWorkingDirPath;
        public CwdProbeRun        launchingJvmSanityDeniedProbe;
        public CwdProbeRun        sessionOwnerDeniedWorkingDirProbe;
        public CwdProbeRun        sessionOwnerAllowedWorkingDirProbe;

        public CwdProbeJvmSnapshot() {
        }
    }

    public static final class NestedSessionOwnerCwdReport implements Serializable {
        private static final long  serialVersionUID = 1L;
        public CwdProbeJvmSnapshot adminCapableTestUserSnapshot;
        public CwdProbeJvmSnapshot elevatedAdminSnapshot;
        public String              nestedFailureStack;

        public NestedSessionOwnerCwdReport() {
        }
    }

    /** LocalSystem: denied dir (must fail for session owner) + allowed profile dir (must succeed) via {@link RunAsHelper#runInSession}. */
    private static final class LocalSystemSessionOwnerWorkingDirTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final int         interactiveSessionId;
        private final String      marker;

        LocalSystemSessionOwnerWorkingDirTask(final int interactiveSessionId, final String marker) {
            this.interactiveSessionId = interactiveSessionId;
            this.marker = marker != null ? marker : "";
        }

        @Override
        public Serializable run() throws Exception {
            if (!WindowsUtils.isRunningAsLocalSystem()) {
                throw new Exception("LocalSystemSessionOwnerWorkingDirTask must run as LocalSystem");
            }
            final CwdProbeJvmSnapshot snapshot = new CwdProbeJvmSnapshot();
            snapshot.isElevated = WindowsUtils.isElevated();
            snapshot.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
            File deniedDir = null;
            File allowedDir = null;
            try {
                // Denied workingDir: ACL denies session owner; launching JVM sanity probe must still succeed.
                deniedDir = createLaunchingJvmWorkingDir(marker + "_DENY");
                restrictDirectoryDenySessionOwner(deniedDir, interactiveSessionId);
                snapshot.deniedWorkingDirPath = deniedDir.getAbsolutePath();
                final java.lang.ProcessBuilder deniedPb = ProcessBuilderFactory.create(powershellCwdProbeArgv());
                deniedPb.directory(deniedDir);
                snapshot.launchingJvmSanityDeniedProbe = captureProbeRun(ProcessBuilderFactory.runCommand(deniedPb));
                // Session owner with denied workingDir must fail.
                final RunAsLaunchOptions deniedOpts = launchOptionsWithWorkingDir(deniedDir);
                snapshot.sessionOwnerDeniedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInSession(interactiveSessionId, powershellCwdProbeArgv(), deniedOpts);
                    }
                });
                // Allowed workingDir under session-owner profile must succeed with matching cwd.
                allowedDir = createSessionOwnerAllowedWorkingDir(interactiveSessionId, marker + "_OK");
                snapshot.sessionOwnerAllowedWorkingDirPath = allowedDir.getAbsolutePath();
                final RunAsLaunchOptions allowedOpts = launchOptionsWithWorkingDir(allowedDir);
                snapshot.sessionOwnerAllowedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInSession(interactiveSessionId, powershellCwdProbeArgv(), allowedOpts);
                    }
                });
                return snapshot;
            } finally {
                deleteTestWorkingDirQuietly(deniedDir);
                deleteTestWorkingDirQuietly(allowedDir);
            }
        }
    }

    /** Elevated admin: same denied/allowed workingDir rules via {@link RunAsHelper#runInOwnerSession}. */
    private static final class ElevatedAdminSessionOwnerWorkingDirTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      marker;

        ElevatedAdminSessionOwnerWorkingDirTask(final String marker) {
            this.marker = marker != null ? marker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final CwdProbeJvmSnapshot snapshot = new CwdProbeJvmSnapshot();
            snapshot.isElevated = WindowsUtils.isElevated();
            snapshot.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
            final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
            File deniedDir = null;
            File allowedDir = null;
            try {
                // Denied workingDir: ACL denies session owner; launching JVM sanity probe must still succeed.
                deniedDir = createLaunchingJvmWorkingDir(marker + "_DENY");
                restrictDirectoryDenySessionOwner(deniedDir, interactiveSessionId);
                snapshot.deniedWorkingDirPath = deniedDir.getAbsolutePath();
                final java.lang.ProcessBuilder deniedPb = ProcessBuilderFactory.create(powershellCwdProbeArgv());
                deniedPb.directory(deniedDir);
                snapshot.launchingJvmSanityDeniedProbe = captureProbeRun(ProcessBuilderFactory.runCommand(deniedPb));
                // Session owner with denied workingDir must fail.
                final RunAsLaunchOptions deniedOpts = launchOptionsWithWorkingDir(deniedDir);
                snapshot.sessionOwnerDeniedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInOwnerSession(powershellCwdProbeArgv(), deniedOpts);
                    }
                });
                // Allowed workingDir under session-owner profile must succeed with matching cwd.
                allowedDir = createSessionOwnerAllowedWorkingDir(interactiveSessionId, marker + "_OK");
                snapshot.sessionOwnerAllowedWorkingDirPath = allowedDir.getAbsolutePath();
                final RunAsLaunchOptions allowedOpts = launchOptionsWithWorkingDir(allowedDir);
                snapshot.sessionOwnerAllowedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInOwnerSession(powershellCwdProbeArgv(), allowedOpts);
                    }
                });
                return snapshot;
            } finally {
                deleteTestWorkingDirQuietly(deniedDir);
                deleteTestWorkingDirQuietly(allowedDir);
            }
        }
    }

    /** Plain non-admin user: {@link RunAsHelper#runInOwnerSession} must fail (privilege), no icacls deny. */
    private static final class PlainUserCannotRunInOwnerSessionTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      marker;

        PlainUserCannotRunInOwnerSessionTask(final String marker) {
            this.marker = marker != null ? marker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final CwdProbeJvmSnapshot snapshot = new CwdProbeJvmSnapshot();
            snapshot.isElevated = WindowsUtils.isElevated();
            snapshot.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
            snapshot.launchingJvmUserName = Advapi32Util.getUserName();
            File workDir = null;
            try {
                workDir = createLaunchingJvmWorkingDir(marker + "_DENY");
                snapshot.deniedWorkingDirPath = workDir.getAbsolutePath();
                final java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(powershellCwdProbeArgv());
                pb.directory(workDir);
                snapshot.launchingJvmSanityDeniedProbe = captureProbeRun(ProcessBuilderFactory.runCommand(pb));
                final RunAsLaunchOptions opts = launchOptionsWithWorkingDir(workDir);
                snapshot.sessionOwnerDeniedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInOwnerSession(powershellCwdProbeArgv(), opts);
                    }
                });
                return snapshot;
            } finally {
                deleteTestWorkingDirQuietly(workDir);
            }
        }
    }

    /**
     * Admin-capable filtered user: {@link RunAsHelper#runInOwnerSession} must fail; then nested elevated admin repeats denied/allowed
     * workingDir rules via {@link RunAsHelper#runInOwnerSession}.
     */
    private static final class AdminCapableUnelevatedThenElevatedWorkingDirTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      marker;

        AdminCapableUnelevatedThenElevatedWorkingDirTask(final String marker) {
            this.marker = marker != null ? marker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final NestedSessionOwnerCwdReport report = new NestedSessionOwnerCwdReport();
            final CwdProbeJvmSnapshot unelevated = new CwdProbeJvmSnapshot();
            unelevated.isElevated = WindowsUtils.isElevated();
            unelevated.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
            File workDir = null;
            try {
                workDir = createLaunchingJvmWorkingDir(marker + "_DENY");
                unelevated.deniedWorkingDirPath = workDir.getAbsolutePath();
                final java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(powershellCwdProbeArgv());
                pb.directory(workDir);
                unelevated.launchingJvmSanityDeniedProbe = captureProbeRun(ProcessBuilderFactory.runCommand(pb));
                final RunAsLaunchOptions opts = launchOptionsWithWorkingDir(workDir);
                unelevated.sessionOwnerDeniedWorkingDirProbe = tryCapture(new CallableLaunch() {
                    @Override
                    public ProcessOutput run() throws Exception {
                        return RunAsHelper.runInOwnerSession(powershellCwdProbeArgv(), opts);
                    }
                });
                report.adminCapableTestUserSnapshot = unelevated;
                try {
                    report.elevatedAdminSnapshot = AdminExecuter.runAsAdmin(new ElevatedAdminSessionOwnerWorkingDirTask(marker), TYPE_CWD_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
                } catch (Throwable t) {
                    report.nestedFailureStack = Exceptions.getStackTrace(t);
                }
                return report;
            } finally {
                deleteTestWorkingDirQuietly(workDir);
            }
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
            if (userName.length() == 0) {
                throw new Exception("CreateTestWindowsUserTask: empty userName");
            }
            final ProcessOutput existsProbe = ProcessBuilderFactory.runCommand("net", "user", userName);
            if (existsProbe.getExitCode() == 0) {
                final ProcessOutput del = ProcessBuilderFactory.runCommand("net", "user", userName, "/delete");
                if (del.getExitCode() != 0) {
                    throw new Exception("net user /delete failed: " + del.getErrOutString());
                }
            }
            final ProcessOutput add = ProcessBuilderFactory.runCommand("net", "user", userName, password, "/add", "/expires:never", "/passwordchg:no");
            if (add.getExitCode() != 0) {
                throw new Exception("net user add failed: " + add.getErrOutString());
            }
            if (addToAdministrators) {
                final String builtinAdminSid = WindowsUtils.SID.SID_BUILTIN_ADMINISTRATORS.sid;
                final Advapi32Util.Account adminGroup = Advapi32Util.getAccountBySid(builtinAdminSid);
                final ProcessOutput groupAdd = ProcessBuilderFactory.runCommand("net", "localgroup", adminGroup.name, userName, "/add");
                if (groupAdd.getExitCode() != 0) {
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
}
