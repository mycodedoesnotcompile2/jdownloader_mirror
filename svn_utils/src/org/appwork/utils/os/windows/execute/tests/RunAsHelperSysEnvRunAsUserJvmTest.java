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
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;

/**
 * Verifies that a JVM started via {@link AdminExecuter#runAsUser} for another account does not inherit the elevated admin task's process
 * environment marker ({@link RunAsHelperSecurityTestSupport#ENV_PROBE_NAME} = {@code APWORK_RUNAS_HELPER_ENV_PROBE}).
 * <p>
 * Probes return the full process env {@link HashMap}; all marker checks run in this host AWTest.
 *
 * <pre>
 * Flow
 *  |
 *  +-- Skip if not Windows or JNA unavailable
 *  |
 *  +-- Create non-admin test user "RunAsJvmEnvUser" (via elevated admin task)
 *  |
 *  +-- Execute task as elevated admin
 *  |     |
 *  |     +-- Step 1: Set APWORK_RUNAS_HELPER_ENV_PROBE = ADMIN_TASK_xxx
 *  |     +-- Step 2: Capture full admin-task process env map
 *  |     +-- Step 3: AdminExecuter.runAsUser(...) launches new JVM as non-admin test user
 *  |           |
 *  |           +-- Step 3a: Capture inherited env map FIRST (do not overwrite yet)
 *  |           +-- Step 3b: Set APWORK_RUNAS_HELPER_ENV_PROBE = NONADMIN_xxx
 *  |           +-- Step 3c: Capture env map again (sanity)
 *  |
 *  +-- Assert report in host JVM
 *  |     |
 *  |     +-- adminTaskElevated == true
 *  |     +-- We expect adminTaskEnv[APWORK_RUNAS_HELPER_ENV_PROBE] == ADMIN_TASK_xxx
 *  |     +-- KEY CHECK: We expect nonAdmin inherited env[APWORK_RUNAS_HELPER_ENV_PROBE] absent or != ADMIN_TASK_xxx
 *  |     +-- We expect nonAdmin own-marker env[APWORK_RUNAS_HELPER_ENV_PROBE] == NONADMIN_xxx
 *  |
 *  +-- Delete test user
 * </pre>
 *
 * The isolation snapshot in {@link NonAdminTestUserEnvProbeTask#run()} is taken <em>before</em> setting the own marker so an inherited
 * admin-task leak would still be visible in the returned map.
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperSysEnvRunAsUserJvmTest extends AWTest {
    private static final String                             USER_RUN_AS          = "RunAsJvmEnvUser";
    private static final TypeRef<RunAsUserJvmEnvReport>     TYPE_REPORT          = new TypeRef<RunAsUserJvmEnvReport>() {
                                                                                   };
    private static final TypeRef<NonAdminTestUserEnvResult> TYPE_NONADMIN_RESULT = new TypeRef<NonAdminTestUserEnvResult>() {
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
        // Step 1: Skip if not Windows or JNA unavailable
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperSysEnvRunAsUserJvmTest: Windows + JNA required, skipped.");
            return;
        }
        final String password = Crypto.generateRandomString(12, "1234567890=)(&%$§!qwerasfdycxbhtnjzmukiliopPOKIUZTREWQASDFGHJHKLMNBVCXY");
        final String adminTaskMarker = "ADMIN_TASK_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        // Step 2: Create non-admin test user
        AdminExecuter.runAsAdmin(new RunAsHelperSecurityTestSupport.CreateTestWindowsUserTask(USER_RUN_AS, password, false), TypeRef.OBJECT, ProcessOptions.DEFAULT);
        try {
            // Step 3: Execute elevated admin task (sets marker, captures env maps, launches runAsUser JVM)
            final RunAsUserJvmEnvReport report = AdminExecuter.runAsAdmin(new ElevatedAdminThenNonAdminTestUserEnvTask(USER_RUN_AS, password, adminTaskMarker), TYPE_REPORT, ProcessOptions.DEFAULT);
            // Elevated admin task must return a report for host-side env checks.
            assertTrue(report != null, "report null");
            // Admin task JVM must be elevated when planting ADMIN_TASK marker.
            assertTrue(report.adminTaskElevated, "admin task JVM must be elevated");
            // Admin-task process env must contain the planted ADMIN_TASK marker.
            assertEnvMarkerEquals("adminTask", adminTaskMarker, report.adminTaskEnv);
            // Inherited non-admin env map is required for the isolation KEY CHECK.
            assertTrue(report.nonAdminTestUserInheritedEnv != null, "nonAdminTestUserInheritedEnv null, err=" + report.nonAdminTestUserError);
            // KEY CHECK: runAsUser JVM must not inherit the admin-task probe marker.
            assertEnvMarkerAbsentOrNotEquals("nonAdminTestUser-inherited", adminTaskMarker, report.nonAdminTestUserInheritedEnv);
            // Sanity: after setting own marker, non-admin env map must be present.
            if (report.nonAdminTestUserOwnMarker != null && report.nonAdminTestUserOwnMarker.length() > 0) {
                // Own-marker capture must have produced an env map.
                assertTrue(report.nonAdminTestUserOwnMarkerEnv != null, "nonAdminTestUserOwnMarkerEnv null");
                // Non-admin JVM must see NONADMIN_xxx after it set its own marker.
                assertEnvMarkerEquals("nonAdminTestUser-own", report.nonAdminTestUserOwnMarker, report.nonAdminTestUserOwnMarkerEnv);
            }
        } finally {
            // Step 7: Delete test user
            RunAsHelperSecurityTestSupport.deleteTestUserQuietly(USER_RUN_AS);
        }
    }

    /** We expect env[APWORK_RUNAS_HELPER_ENV_PROBE] == expectedMarker. */
    private void assertEnvMarkerEquals(final String label, final String expectedMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the probe dump is required to read the marker.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        // Probe variable must be present when we expect the planted marker.
        assertTrue(value != null, label + ": expected " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "==" + expectedMarker + ", got absent");
        // Marker value must exactly match what the process set.
        assertEquals(expectedMarker, value, label + ": " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
    }

    /**
     * Pass if APWORK_RUNAS_HELPER_ENV_PROBE is absent/empty, or present with a value other than {@code adminTaskMarker}. Fail only if the
     * inherited non-admin env equals the admin-task marker (env leak).
     */
    private void assertEnvMarkerAbsentOrNotEquals(final String label, final String adminTaskMarker, final HashMap<String, String> env) throws Exception {
        // Inherited env map is required to detect an admin-task marker leak.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        if (value == null) {
            return;
        }
        // Non-admin JVM must not still carry the admin-task probe marker value.
        assertTrue(!adminTaskMarker.equals(value), label + ": must not see admin-task marker, " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "=" + value);
    }

    public static final class RunAsUserJvmEnvReport implements Serializable {
        private static final long         serialVersionUID = 1L;
        public boolean                    adminTaskElevated;
        /** Full admin-task process env after setting ADMIN_TASK marker. */
        public HashMap<String, String>    adminTaskEnv;
        /** Inherited env of non-admin JVM (before own marker). */
        public HashMap<String, String>    nonAdminTestUserInheritedEnv;
        /** Env of non-admin JVM after setting {@link #nonAdminTestUserOwnMarker}. */
        public HashMap<String, String>    nonAdminTestUserOwnMarkerEnv;
        public String                     nonAdminTestUserOwnMarker;
        public String                     nonAdminTestUserError;

        public RunAsUserJvmEnvReport() {
        }
    }

    public static final class NonAdminTestUserEnvResult implements Serializable {
        private static final long      serialVersionUID = 1L;
        public HashMap<String, String> inheritedEnv;
        public HashMap<String, String> ownMarkerEnv;

        public NonAdminTestUserEnvResult() {
        }
    }

    private static final class ElevatedAdminThenNonAdminTestUserEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      userName;
        private final String      password;
        private final String      adminTaskMarker;

        ElevatedAdminThenNonAdminTestUserEnvTask(final String userName, final String password, final String adminTaskMarker) {
            this.userName = userName != null ? userName : "";
            this.password = password != null ? password : "";
            this.adminTaskMarker = adminTaskMarker != null ? adminTaskMarker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final RunAsUserJvmEnvReport report = new RunAsUserJvmEnvReport();
            // Step 1: Record elevation state of admin task JVM
            report.adminTaskElevated = WindowsUtils.isElevated();
            // Step 2: Set APWORK_RUNAS_HELPER_ENV_PROBE = ADMIN_TASK_xxx in admin task process
            RunAsHelperSecurityTestSupport.setProcessEnvironmentVariable(RunAsHelperSecurityTestSupport.ENV_PROBE_NAME, adminTaskMarker);
            // Step 3: Capture full admin-task env map (host will assert marker present)
            report.adminTaskEnv = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvironment();
            try {
                final String ownMarker = "NONADMIN_" + Crypto.generateRandomString(6, "abcdefghijklmnopqrstuvwxyz0123456789");
                report.nonAdminTestUserOwnMarker = ownMarker;
                // Step 4: Launch separate JVM as non-admin test user (returns env maps; host evaluates)
                final NonAdminTestUserEnvResult result = AdminExecuter.runAsUser(null, userName, password, new NonAdminTestUserEnvProbeTask(ownMarker), TYPE_NONADMIN_RESULT, ProcessOptions.DEFAULT);
                if (result != null) {
                    report.nonAdminTestUserInheritedEnv = result.inheritedEnv;
                    report.nonAdminTestUserOwnMarkerEnv = result.ownMarkerEnv;
                }
            } catch (Throwable t) {
                report.nonAdminTestUserError = t.getMessage();
                report.nonAdminTestUserInheritedEnv = new HashMap<String, String>();
            }
            return report;
        }
    }

    /**
     * Capture inherited process env map first (isolation), then set own marker and capture again (sanity). Overwriting before the
     * isolation snapshot would hide an admin-task env leak.
     */
    private static final class NonAdminTestUserEnvProbeTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      ownMarker;

        NonAdminTestUserEnvProbeTask(final String ownMarker) {
            this.ownMarker = ownMarker != null ? ownMarker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final NonAdminTestUserEnvResult result = new NonAdminTestUserEnvResult();
            // Step 1: Capture inherited env map FIRST — host expects APWORK_RUNAS_HELPER_ENV_PROBE not equal ADMIN_TASK_xxx
            result.inheritedEnv = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvironment();
            // Step 2: Set own marker in non-admin JVM (must not run before Step 1 or admin leak would be hidden)
            RunAsHelperSecurityTestSupport.setProcessEnvironmentVariable(RunAsHelperSecurityTestSupport.ENV_PROBE_NAME, ownMarker);
            // Step 3: Capture env map again — host expects APWORK_RUNAS_HELPER_ENV_PROBE == NONADMIN_xxx
            result.ownMarkerEnv = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvironment();
            return result;
        }
    }
}
