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

import java.io.Serializable;
import java.util.EnumSet;
import java.util.HashMap;
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
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.platform.win32.Advapi32Util;

/**
 * Four scenarios verifying that a session-owner child launched via {@link RunAsHelper} does not inherit the launching JVM's process environment
 * marker. Tasks collect raw probe data only; all pass/fail checks run in this {@link AWTest} host JVM.
 * <p>
 * Probes return the full process environment {@link HashMap}; the host evaluates {@link RunAsHelperSecurityTestSupport#ENV_PROBE_NAME}
 * ({@code APWORK_RUNAS_HELPER_ENV_PROBE}).
 *
 * <pre>
 * Scenario 1 — Local System to session owner
 *  |
 *  +-- Execute task as NT AUTHORITY\SYSTEM
 *  +-- Set APWORK_RUNAS_HELPER_ENV_PROBE = LOCAL_SYSTEM_xxx in launching JVM
 *  +-- Launch session owner via RunAsHelper.runInSession(interactive WTS session)
 *  +-- We expect APWORK_RUNAS_HELPER_ENV_PROBE not present or not equal LOCAL_SYSTEM_xxx in session-owner child
 *
 * Scenario 2 — UAC elevated admin to session owner (same interactive user, de-elevated child)
 *  |
 *  +-- Execute task as elevated admin
 *  +-- Set APWORK_RUNAS_HELPER_ENV_PROBE = ELEVATED_ADMIN_xxx in launching JVM
 *  +-- Launch session owner via RunAsHelper.runInOwnerSession
 *  +-- We expect APWORK_RUNAS_HELPER_ENV_PROBE not present or not equal ELEVATED_ADMIN_xxx in session-owner child
 *
 * Scenario 3 — Plain test user must not launch session owner
 *  |
 *  +-- Create non-admin test user "RunAsEnvPlainUser"
 *  +-- Execute task as plain test user via AdminExecuter.runAsUser
 *  +-- Set APWORK_RUNAS_HELPER_ENV_PROBE = PLAIN_TEST_USER_xxx in launching JVM
 *  +-- Attempt RunAsHelper.runInOwnerSession (expect Win32 access/privilege failure)
 *  +-- We expect session-owner launch to fail (non-zero exit, stderr with access denied / privilege error)
 *  +-- Delete test user
 *
 * Scenario 4 — Admin-capable test user via nested UAC to session owner
 *  |
 *  +-- Create admin-group test user "RunAsEnvTestUser"
 *  +-- Execute task as test user via AdminExecuter.runAsUser (filtered token, not elevated)
 *  +-- Set APWORK_RUNAS_HELPER_ENV_PROBE = ADMIN_CAPABLE_TEST_USER_xxx in runAsUser JVM
 *  +-- Nested AdminExecuter.runAsAdmin elevates, then RunAsHelper.runInOwnerSession
 *  +-- We expect APWORK_RUNAS_HELPER_ENV_PROBE not present or not equal ADMIN_CAPABLE_TEST_USER_xxx in session-owner child
 *  +-- Delete test user
 * </pre>
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperSysEnvTest extends AWTest {
    private static final String                USER_PLAIN               = "RunAsEnvPlainUser";
    private static final String                USER_ADMIN_CAPABLE       = "RunAsEnvTestUser";
    private static final TypeRef<EnvProbeJvmSnapshot> TYPE_ENV_PROBE_JVM_SNAPSHOT = new TypeRef<EnvProbeJvmSnapshot>() {
                                                                                    };
    private static final TypeRef<NestedSessionOwnerEnvReport> TYPE_NESTED_SESSION_OWNER_ENV_REPORT = new TypeRef<NestedSessionOwnerEnvReport>() {
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
            logInfoAnyway("RunAsHelperSysEnvTest: Windows only, skipped.");
            testSkipped("LocalSystem->SessionOwner", "Requires Windows");
            testSkipped("ElevatedAdmin->SessionOwner", "Requires Windows");
            testSkipped("PlainTestUser->SessionOwner", "Requires Windows");
            testSkipped("ElevatedTestUser->SessionOwner", "Requires Windows");
            return;
        }
        if (!JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperSysEnvTest: JNA not available, skipped.");
            testSkipped("LocalSystem->SessionOwner", "JNA not available");
            testSkipped("ElevatedAdmin->SessionOwner", "JNA not available");
            testSkipped("PlainTestUser->SessionOwner", "JNA not available");
            testSkipped("ElevatedTestUser->SessionOwner", "JNA not available");
            return;
        }
        final int interactiveSessionId = WindowsUtils.getCurrentProcessSessionId();
        if (interactiveSessionId < 0) {
            logInfoAnyway("RunAsHelperSysEnvTest: skip all scenarios (no interactive WTS session id).");
            testSkipped("LocalSystem->SessionOwner", "No interactive WTS session");
            testSkipped("ElevatedAdmin->SessionOwner", "No interactive WTS session");
            testSkipped("PlainTestUser->SessionOwner", "No interactive WTS session");
            testSkipped("ElevatedTestUser->SessionOwner", "No interactive WTS session");
            return;
        }
        test01LocalSystemToSessionOwnerEnvDoesNotLeak(interactiveSessionId);
        testSucceeded("LocalSystem->SessionOwner");
        test02ElevatedAdminToSessionOwnerEnvDoesNotLeak();
        testSucceeded("ElevatedAdmin->SessionOwner");
        test03PlainTestUserSessionOwnerLaunchMustFail();
        testSucceeded("PlainTestUser->SessionOwner");
        test04AdminCapableTestUserToSessionOwnerEnvDoesNotLeak();
        testSucceeded("ElevatedTestUser->SessionOwner");
    }

    /** Scenario 1: Local System launching JVM to interactive session owner. We expect env marker not to leak into session-owner child. */
    private void test01LocalSystemToSessionOwnerEnvDoesNotLeak(final int interactiveSessionId) throws Exception {
        final String marker = "LOCAL_SYSTEM_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        // Step 1: Execute task as Local System — sets marker, launches session owner, returns probe snapshot
        final EnvProbeJvmSnapshot snapshot = AdminExecuter.runAsLocalSystem(new LocalSystemToSessionOwnerEnvTask(interactiveSessionId, marker), TYPE_ENV_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        // Step 2: Assert launching JVM sanity + session-owner isolation
        assertEnvIsolationScenario("01-local-system->session-owner", marker, snapshot, null, true);
    }

    /**
     * Scenario 2: UAC elevated admin launching JVM to non-elevated session owner (same interactive user).
     * We expect APWORK_RUNAS_HELPER_ENV_PROBE not present or not equal ELEVATED_ADMIN_xxx in session-owner child.
     */
    private void test02ElevatedAdminToSessionOwnerEnvDoesNotLeak() throws Exception {
        final String marker = "ELEVATED_ADMIN_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        // Step 1: Execute task as elevated admin — sets marker, launches session owner via runInOwnerSession
        final EnvProbeJvmSnapshot snapshot = AdminExecuter.runAsAdmin(new ElevatedAdminToSessionOwnerEnvTask(marker, OwnerLaunchMode.CURRENT_SESSION), TYPE_ENV_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
        // Step 2: Assert launching JVM elevated + session-owner isolation
        assertEnvIsolationScenario("02-elevated-admin->session-owner", marker, snapshot, Boolean.TRUE, false);
    }

    /**
     * Scenario 3: Plain local test user must not be able to launch session owner.
     * We expect RunAsHelper.runInOwnerSession to fail with Win32 access/privilege error.
     */
    private void test03PlainTestUserSessionOwnerLaunchMustFail() throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        // Step 1: Create non-admin test user
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_PLAIN, password, false), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final String marker = "PLAIN_TEST_USER_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
            // Step 2: Execute task as plain test user — sets marker, attempts session-owner launch
            final EnvProbeJvmSnapshot snapshot = AdminExecuter.runAsUser(null, USER_PLAIN, password, new PlainTestUserToSessionOwnerEnvTask(marker, OwnerLaunchMode.CURRENT_SESSION), TYPE_ENV_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
            // Step 3: We expect session-owner launch to fail (not succeed)
            assertPlainTestUserSessionOwnerLaunchRejected("03-plain-testuser->session-owner", marker, snapshot);
        } finally {
            // Step 4: Delete test user
            deleteTestUserQuietly(USER_PLAIN);
        }
    }

    /**
     * Scenario 4: Admin-capable test user via runAsUser + nested UAC elevation to session owner.
     * We expect APWORK_RUNAS_HELPER_ENV_PROBE not present or not equal ADMIN_CAPABLE_TEST_USER_xxx in session-owner child.
     */
    private void test04AdminCapableTestUserToSessionOwnerEnvDoesNotLeak() throws Exception {
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        // Step 1: Create admin-group test user
        AdminExecuter.runAsAdmin(new CreateTestWindowsUserTask(USER_ADMIN_CAPABLE, password, true), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            final String marker = "ADMIN_CAPABLE_TEST_USER_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
            // Step 2: Execute as test user — nested runAsAdmin elevates and launches session owner
            final NestedSessionOwnerEnvReport report = AdminExecuter.runAsUser(null, USER_ADMIN_CAPABLE, password, new AdminCapableTestUserToSessionOwnerEnvTask(marker), TYPE_NESTED_SESSION_OWNER_ENV_REPORT, ProcessOptions.DEFAULT);
            // Step 3: Assert runAsUser JVM sanity + nested elevated session-owner isolation
            assertNestedSessionOwnerEnvReport("04-admin-capable-testuser->session-owner", marker, report, false);
        } finally {
            // Step 4: Delete test user
            deleteTestUserQuietly(USER_ADMIN_CAPABLE);
        }
    }

    private void deleteTestUserQuietly(final String userName) {
        try {
            AdminExecuter.runAsAdmin(new DeleteTestWindowsUserTask(userName), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        } catch (Throwable t) {
            logInfoAnyway("RunAsHelperSysEnvTest: cleanup net user delete failed (" + userName + "): " + t.getMessage());
        }
    }

    /**
     * @param expectElevated
     *            {@code null} = do not check {@link EnvProbeJvmSnapshot#isElevated} (Local System may report elevated due to high integrity)
     * @param expectLocalSystem
     *            when {@code true}, {@link EnvProbeJvmSnapshot#isLocalSystem} must be true
     */
    private void assertEnvIsolationScenario(final String label, final String expectedMarker, final EnvProbeJvmSnapshot snapshot, final Boolean expectElevated, final boolean expectLocalSystem) throws Exception {
        // Task must have returned a probe snapshot for host-side evaluation.
        assertTrue(snapshot != null, label + ": snapshot is null");
        if (expectLocalSystem) {
            // We expect launching JVM to be Local System
            assertTrue(snapshot.isLocalSystem, label + ": launching JVM must be Local System");
        }
        // We expect launching JVM to have set and probed its own marker
        assertLaunchingJvmEnvSanity(label, expectedMarker, snapshot, expectElevated);
        // We expect session-owner child not to inherit launching JVM marker
        assertSessionOwnerEnvIsolated(label, expectedMarker, snapshot);
    }

    /** Scenario 4: runAsUser launching JVM snapshot + nested elevated session-owner launch. */
    private void assertNestedSessionOwnerEnvReport(final String label, final String expectedMarker, final NestedSessionOwnerEnvReport report, final boolean expectLocalSystem) throws Exception {
        // Nested scenario must return a report from the runAsUser JVM.
        assertTrue(report != null, label + ": report is null");
        // Report marker must match the value we planted in the runAsUser JVM.
        assertEquals(expectedMarker, report.launchingJvmMarker, label + ": launchingJvmMarker");
        // Filtered-token runAsUser probe snapshot is required before nested elevation checks.
        assertTrue(report.adminCapableTestUserSnapshot != null, label + ": adminCapableTestUserSnapshot missing");
        // We expect runAsUser JVM not elevated (filtered admin token)
        assertFalse(report.adminCapableTestUserSnapshot.isElevated, label + ": runAsUser JVM must not be elevated (filtered admin token)");
        // runAsUser JVM must still see its own planted env marker.
        assertLaunchingJvmEnvSanity(label + "/runAsUser", expectedMarker, report.adminCapableTestUserSnapshot, Boolean.FALSE);
        // Nested elevated task must have produced a session-owner snapshot.
        assertTrue(report.elevatedAdminSnapshot != null, label + ": elevated session-owner launch missing, nested failure:\n" + report.nestedFailureStack);
        // Nested runAsAdmin must succeed — empty failure stack.
        assertTrue(report.nestedFailureStack == null || report.nestedFailureStack.trim().length() == 0, label + ": nested runAsAdmin failed:\n" + report.nestedFailureStack);
        // We expect nested elevated launch also isolates env from launching marker
        assertEnvIsolationScenario(label + "/elevated", expectedMarker, report.elevatedAdminSnapshot, Boolean.TRUE, expectLocalSystem);
    }

    /** We expect launching JVM env map to contain APWORK_RUNAS_HELPER_ENV_PROBE == expectedMarker. */
    private void assertLaunchingJvmEnvSanity(final String label, final String expectedMarker, final EnvProbeJvmSnapshot snapshot, final Boolean expectElevated) throws Exception {
        // Launching-JVM sanity checks need a non-null snapshot.
        assertTrue(snapshot != null, label + ": snapshot is null");
        if (expectElevated != null) {
            // Elevation flag must match the scenario (elevated admin vs filtered token).
            assertEquals(expectElevated.booleanValue(), snapshot.isElevated, label + ": isElevated (WindowsUtils.isElevated)");
        }
        // Snapshot must echo the marker we asked the launching JVM to set.
        assertEquals(expectedMarker, snapshot.launchingJvmMarker, label + ": launchingJvmMarker");
        // Launching-JVM env dump process must complete cleanly.
        assertProbeRunProcessOk(label + "/launchingJvm", snapshot.launchingJvmProbe);
        // Launching JVM process env must contain the planted probe marker.
        assertEnvMarkerEquals(label + "/launchingJvm", expectedMarker, snapshot.launchingJvmProbe.env);
    }

    /** We expect session-owner env map: APWORK_RUNAS_HELPER_ENV_PROBE absent/empty or not equal to launching JVM marker (no env leak). */
    private void assertSessionOwnerEnvIsolated(final String label, final String expectedMarker, final EnvProbeJvmSnapshot snapshot) throws Exception {
        // Isolation checks need the launching-JVM snapshot that holds the child probe.
        assertTrue(snapshot != null, label + ": snapshot is null");
        // Session-owner env dump must succeed so we can evaluate marker isolation.
        assertProbeRunProcessOk(label + "/sessionOwner", snapshot.sessionOwnerProbe);
        // Token launch must not leak the launching JVM's env probe marker into the child.
        assertEnvMarkerAbsentOrNotEquals(label + "/sessionOwner", expectedMarker, snapshot.sessionOwnerProbe.env);
    }

    /** Plain user scenario: We expect session-owner launch to fail (non-zero exit, stderr with access/privilege error). */
    private void assertPlainTestUserSessionOwnerLaunchRejected(final String label, final String expectedMarker, final EnvProbeJvmSnapshot snapshot) throws Exception {
        // Plain-user rejection checks need a returned snapshot.
        assertTrue(snapshot != null, label + ": snapshot is null");
        // Plain test user JVM must not be elevated — otherwise privilege assumptions break.
        assertFalse(snapshot.isElevated, label + ": plain test user launching JVM must not be elevated");
        // Plain user must still see its own planted marker before the failed launch.
        assertLaunchingJvmEnvSanity(label, expectedMarker, snapshot, Boolean.FALSE);
        // Failed launch path must still populate a session-owner probe (exit/stderr).
        assertTrue(snapshot.sessionOwnerProbe != null, label + ": sessionOwnerProbe is null");
        // Plain user must not successfully launch a session-owner child.
        assertTrue(snapshot.sessionOwnerProbe.exitCode != 0, label + ": session-owner launch must not succeed, exit=" + snapshot.sessionOwnerProbe.exitCode + ", stdout=" + snapshot.sessionOwnerProbe.stdout);
        // Failure must leave an exception message on stderr for diagnosis.
        assertTrue(!stderrBlank(snapshot.sessionOwnerProbe.stderr), label + ": session-owner launch must record exception, stderr empty");
        // Stderr must indicate Win32 access/privilege rejection, not an unrelated failure.
        assertTrue(stderrIndicatesSessionOwnerLaunchRejected(snapshot.sessionOwnerProbe.stderr), label + ": expected Win32/access failure in stderr, got: " + snapshot.sessionOwnerProbe.stderr);
    }

    /** We expect env[APWORK_RUNAS_HELPER_ENV_PROBE] == expectedMarker. */
    private void assertEnvMarkerEquals(final String label, final String expectedMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the probe dump is required to read the marker.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        // Probe variable must be present when we expect the planted marker.
        assertTrue(value != null, label + ": expected " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "==" + expectedMarker + ", got absent");
        // Marker value must exactly match what the launching process set.
        assertEquals(expectedMarker, value, label + ": " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
    }

    /**
     * Pass if APWORK_RUNAS_HELPER_ENV_PROBE is absent/empty, or present with a value other than {@code launchingJvmMarker}. Fail only if the
     * session-owner env equals the launching JVM marker (env leak).
     */
    private void assertEnvMarkerAbsentOrNotEquals(final String label, final String launchingJvmMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the child probe is required to detect a leak.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        if (value == null) {
            return;
        }
        // Child must not carry the launching JVM's exact probe marker value (env leak).
        assertTrue(!launchingJvmMarker.equals(value), label + ": session-owner child must not inherit launching JVM env, " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "=" + value);
    }

    private static boolean stderrIndicatesSessionOwnerLaunchRejected(final String stderr) {
        if (stderr == null) {
            return false;
        }
        final String s = stderr.toLowerCase();
        return s.indexOf("win32exception") >= 0 || s.indexOf("zugriff verweigert") >= 0 || s.indexOf("access denied") >= 0 || s.indexOf("privilege") >= 0 || s.indexOf("error_privilege_not_held") >= 0 || s.indexOf(" 1314") >= 0
                || s.indexOf("createenvironmentblock") >= 0 || s.indexOf("createprocesswithtoken") >= 0 || s.indexOf("createprocessasuser") >= 0 || s.indexOf("runtasprocesslauncher") >= 0 || s.indexOf("runashelper") >= 0;
    }

    private void assertProbeRunProcessOk(final String label, final RunAsHelperSecurityTestSupport.EnvProbeRun run) throws Exception {
        // Probe run object must exist before checking exit/stderr/env.
        assertTrue(run != null, label + ": probe run is null");
        // Env dump process must exit 0 so the map is trustworthy.
        assertEquals(0, run.exitCode, label + ": exitCode, stderr=" + run.stderr + ", stdout=" + run.stdout);
        // Clean probe: no unexpected stderr from the dump process.
        assertTrue(stderrBlank(run.stderr), label + ": stderr not empty: " + run.stderr);
        // Successful dump must include a parsed environment map.
        assertTrue(run.env != null, label + ": env map missing");
    }

    private static boolean stderrBlank(final String stderr) {
        return stderr == null || stderr.trim().length() == 0;
    }

    /** Sets APWORK_RUNAS_HELPER_ENV_PROBE in launching JVM and captures full process env map. */
    private static EnvProbeJvmSnapshot captureLaunchingJvmEnvSnapshot(final String launchingJvmMarker) throws Exception {
        final EnvProbeJvmSnapshot snapshot = new EnvProbeJvmSnapshot();
        snapshot.isElevated = WindowsUtils.isElevated();
        snapshot.isLocalSystem = WindowsUtils.isRunningAsLocalSystem();
        snapshot.launchingJvmMarker = launchingJvmMarker;
        // Step 1: Set APWORK_RUNAS_HELPER_ENV_PROBE = launchingJvmMarker in launching JVM process
        RunAsHelperSecurityTestSupport.setProcessEnvironmentVariable(RunAsHelperSecurityTestSupport.ENV_PROBE_NAME, launchingJvmMarker);
        // Step 2: Capture full launching JVM process env map (native GetEnvironmentStrings)
        snapshot.launchingJvmProbe = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvProbe();
        return snapshot;
    }

    /** Launches session-owner child with full env dump — host evaluates APWORK_RUNAS_HELPER_ENV_PROBE isolation. */
    private static void launchSessionOwnerEnvProbe(final EnvProbeJvmSnapshot snapshot, final OwnerLaunchMode mode, final int interactiveSessionId) {
        try {
            final ProcessOutput sessionOwnerOut;
            // Step 1: Launch session-owner child via RunAsHelper (token path, not ProcessBuilder inheritance)
            if (mode == OwnerLaunchMode.INTERACTIVE_SESSION) {
                sessionOwnerOut = RunAsHelper.runInSession(interactiveSessionId, RunAsHelperSecurityTestSupport.powershellEnvDumpAllArgv(), RunAsLaunchOptions.DEFAULT);
            } else {
                sessionOwnerOut = RunAsHelper.runInOwnerSession(RunAsHelperSecurityTestSupport.powershellEnvDumpAllArgv(), RunAsLaunchOptions.DEFAULT);
            }
            // Step 2: Capture session-owner full env map (success path)
            snapshot.sessionOwnerProbe = RunAsHelperSecurityTestSupport.captureEnvMapProbe(sessionOwnerOut);
        } catch (Throwable t) {
            // Step 2 alt: Capture failure (expected for plain test user scenario)
            snapshot.sessionOwnerProbe = RunAsHelperSecurityTestSupport.captureEnvMapProbeFromThrowable(t);
        }
    }

    private enum OwnerLaunchMode {
        CURRENT_SESSION,
        INTERACTIVE_SESSION
    }

    public static final class EnvProbeJvmSnapshot implements Serializable {
        private static final long serialVersionUID = 1L;
        public boolean                                      isElevated;
        public boolean                                      isLocalSystem;
        public String                                       launchingJvmMarker;
        public RunAsHelperSecurityTestSupport.EnvProbeRun launchingJvmProbe;
        public RunAsHelperSecurityTestSupport.EnvProbeRun sessionOwnerProbe;

        public EnvProbeJvmSnapshot() {
        }
    }

    public static final class NestedSessionOwnerEnvReport implements Serializable {
        private static final long serialVersionUID = 1L;
        public String                launchingJvmMarker;
        public EnvProbeJvmSnapshot   adminCapableTestUserSnapshot;
        public EnvProbeJvmSnapshot   elevatedAdminSnapshot;
        public String                nestedFailureStack;

        public NestedSessionOwnerEnvReport() {
        }
    }

    private static final class LocalSystemToSessionOwnerEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final int         interactiveSessionId;
        private final String      launchingJvmMarker;

        LocalSystemToSessionOwnerEnvTask(final int interactiveSessionId, final String launchingJvmMarker) {
            this.interactiveSessionId = interactiveSessionId;
            this.launchingJvmMarker = launchingJvmMarker != null ? launchingJvmMarker : "";
        }

        @Override
        public Serializable run() throws Exception {
            if (!WindowsUtils.isRunningAsLocalSystem()) {
                throw new Exception("LocalSystemToSessionOwnerEnvTask must run as NT AUTHORITY\\SYSTEM");
            }
            // Step 1: Set marker and probe launching JVM env
            final EnvProbeJvmSnapshot snapshot = captureLaunchingJvmEnvSnapshot(launchingJvmMarker);
            // Step 2: Launch session owner in interactive WTS session and probe child env
            launchSessionOwnerEnvProbe(snapshot, OwnerLaunchMode.INTERACTIVE_SESSION, interactiveSessionId);
            return snapshot;
        }
    }

    /** Elevated launching JVM: set marker, {@link RunAsHelper#runInOwnerSession} or {@link RunAsHelper#runInSession}. */
    private static final class ElevatedAdminToSessionOwnerEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String            launchingJvmMarker;
        private final OwnerLaunchMode   launchMode;
        private final int               interactiveSessionId;

        ElevatedAdminToSessionOwnerEnvTask(final String launchingJvmMarker, final OwnerLaunchMode launchMode) {
            this(launchingJvmMarker, launchMode, -1);
        }

        ElevatedAdminToSessionOwnerEnvTask(final String launchingJvmMarker, final OwnerLaunchMode launchMode, final int interactiveSessionId) {
            this.launchingJvmMarker = launchingJvmMarker != null ? launchingJvmMarker : "";
            this.launchMode = launchMode != null ? launchMode : OwnerLaunchMode.CURRENT_SESSION;
            this.interactiveSessionId = interactiveSessionId;
        }

        @Override
        public Serializable run() throws Exception {
            // Step 1: Set marker and probe launching JVM env
            final EnvProbeJvmSnapshot snapshot = captureLaunchingJvmEnvSnapshot(launchingJvmMarker);
            // Step 2: Launch session owner and probe child env
            launchSessionOwnerEnvProbe(snapshot, launchMode, interactiveSessionId);
            return snapshot;
        }
    }

    /** {@link AdminExecuter#runAsUser} launching JVM: set marker, direct session-owner launch (no nested UAC). */
    private static final class PlainTestUserToSessionOwnerEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String          launchingJvmMarker;
        private final OwnerLaunchMode launchMode;

        PlainTestUserToSessionOwnerEnvTask(final String launchingJvmMarker, final OwnerLaunchMode launchMode) {
            this.launchingJvmMarker = launchingJvmMarker != null ? launchingJvmMarker : "";
            this.launchMode = launchMode != null ? launchMode : OwnerLaunchMode.CURRENT_SESSION;
        }

        @Override
        public Serializable run() throws Exception {
            // Step 1: Set marker and probe plain test user launching JVM env
            final EnvProbeJvmSnapshot snapshot = captureLaunchingJvmEnvSnapshot(launchingJvmMarker);
            // Step 2: Attempt session-owner launch — We expect Win32 access/privilege failure
            launchSessionOwnerEnvProbe(snapshot, launchMode, -1);
            return snapshot;
        }
    }

    /** {@link AdminExecuter#runAsUser} + nested {@link AdminExecuter#runAsAdmin} + session-owner launch. */
    private static final class AdminCapableTestUserToSessionOwnerEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      launchingJvmMarker;

        AdminCapableTestUserToSessionOwnerEnvTask(final String launchingJvmMarker) {
            this.launchingJvmMarker = launchingJvmMarker != null ? launchingJvmMarker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final NestedSessionOwnerEnvReport report = new NestedSessionOwnerEnvReport();
            report.launchingJvmMarker = launchingJvmMarker;
            // Step 1: Set marker and probe runAsUser launching JVM env (filtered token, not elevated)
            report.adminCapableTestUserSnapshot = captureLaunchingJvmEnvSnapshot(launchingJvmMarker);
            try {
                // Step 2: Nested elevated admin task launches session owner
                report.elevatedAdminSnapshot = AdminExecuter.runAsAdmin(new ElevatedAdminToSessionOwnerEnvTask(launchingJvmMarker, OwnerLaunchMode.CURRENT_SESSION), TYPE_ENV_PROBE_JVM_SNAPSHOT, ProcessOptions.DEFAULT);
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
                final String builtinAdminSid = WindowsUtils.SID.SID_BUILTIN_ADMINISTRATORS.sid;
                final Advapi32Util.Account adminGroup = Advapi32Util.getAccountBySid(builtinAdminSid);
                final ProcessOutput groupAdd = ProcessBuilderFactory.runCommand("net", "localgroup", adminGroup.name, userName, "/add");
                if (groupAdd.getExitCode() != 0) {
                    throw new Exception("net localgroup add failed: " + groupAdd.getErrOutString() + " " + groupAdd.getStdOutString());
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
