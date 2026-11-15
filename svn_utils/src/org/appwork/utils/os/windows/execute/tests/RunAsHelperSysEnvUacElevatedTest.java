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
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessOutput;

/**
 * Verifies env isolation for {@link RunAsHelper#runUACElevated} from an already elevated JVM: uses owner-session token launch (no ShellExecute
 * {@code runas}). We expect the de-elevated session-owner child not to inherit the elevated admin marker
 * ({@link RunAsHelperSecurityTestSupport#ENV_PROBE_NAME} = {@code APWORK_RUNAS_HELPER_ENV_PROBE}).
 * <p>
 * Probes return full env maps; host evaluates markers.
 *
 * <pre>
 * Flow
 *  |
 *  +-- Skip if not Windows/JNA or no WTS session
 *  |
 *  +-- Execute task as elevated admin
 *  |     |
 *  |     +-- Step 1: Set APWORK_RUNAS_HELPER_ENV_PROBE = ELEVATED_ADMIN_UAC_xxx
 *  |     +-- Step 2: Capture elevated admin env map
 *  |     +-- Step 3: RunAsHelper.runUACElevated launches de-elevated session-owner child (env dump-all)
 *  |
 *  +-- We expect elevatedAdminEnv[APWORK_RUNAS_HELPER_ENV_PROBE] == ELEVATED_ADMIN_UAC_xxx
 *  +-- We expect sessionOwnerEnv[APWORK_RUNAS_HELPER_ENV_PROBE] absent or != ELEVATED_ADMIN_UAC_xxx
 * </pre>
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperSysEnvUacElevatedTest extends AWTest {
    private static final TypeRef<UacElevatedEnvSnapshot> TYPE_SNAPSHOT = new TypeRef<UacElevatedEnvSnapshot>() {
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
        // Step 1: Skip if not Windows/JNA or no WTS session
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperSysEnvUacElevatedTest: Windows + JNA required, skipped.");
            return;
        }
        if (WindowsUtils.getCurrentProcessSessionId() < 0) {
            logInfoAnyway("RunAsHelperSysEnvUacElevatedTest: no WTS session, skipped.");
            return;
        }
        final String marker = "ELEVATED_ADMIN_UAC_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        // Step 2: Execute elevated admin task (sets marker, runUACElevated, returns env maps)
        final UacElevatedEnvSnapshot snapshot = AdminExecuter.runAsAdmin(new ElevatedAdminRunUacElevatedEnvTask(marker), TYPE_SNAPSHOT, ProcessOptions.DEFAULT);
        // Elevated runUACElevated task must return a snapshot for host-side checks.
        assertTrue(snapshot != null, "snapshot null");
        // Task JVM must already be elevated — runUACElevated de-elevates the child from that context.
        assertTrue(snapshot.isElevated, "task JVM must be elevated");
        // Elevated admin JVM must still hold the planted UAC env marker.
        assertEnvMarkerEquals("elevatedAdmin", marker, snapshot.elevatedAdminEnv);
        // De-elevated session-owner probe must be present after runUACElevated.
        assertTrue(snapshot.sessionOwnerProbe != null, "sessionOwnerProbe null");
        // Session-owner env dump must succeed so isolation can be evaluated.
        assertEquals(0, snapshot.sessionOwnerProbe.exitCode, "sessionOwner exit, stderr=" + snapshot.sessionOwnerProbe.stderr);
        // De-elevated child must not inherit the elevated admin probe marker.
        assertEnvMarkerAbsentOrNotEquals("sessionOwner-runUACElevated", marker, snapshot.sessionOwnerProbe.env);
    }

    private void assertEnvMarkerEquals(final String label, final String expectedMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the probe dump is required to read the marker.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        // Probe variable must be present when we expect the planted marker.
        assertTrue(value != null, label + ": expected " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "==" + expectedMarker + ", got absent");
        // Marker value must exactly match what the launching process set.
        assertEquals(expectedMarker, value, label + ": marker");
    }

    private void assertEnvMarkerAbsentOrNotEquals(final String label, final String launchingJvmMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the child probe is required to detect a leak.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        if (value == null) {
            return;
        }
        // Child must not carry the elevated admin probe marker value (env leak).
        assertTrue(!launchingJvmMarker.equals(value), label + ": env leak, " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "=" + value);
    }

    public static final class UacElevatedEnvSnapshot implements Serializable {
        private static final long serialVersionUID = 1L;
        public boolean                                       isElevated;
        public HashMap<String, String>                       elevatedAdminEnv;
        public RunAsHelperSecurityTestSupport.EnvProbeRun   sessionOwnerProbe;

        public UacElevatedEnvSnapshot() {
        }
    }

    private static final class ElevatedAdminRunUacElevatedEnvTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;
        private final String      marker;

        ElevatedAdminRunUacElevatedEnvTask(final String marker) {
            this.marker = marker != null ? marker : "";
        }

        @Override
        public Serializable run() throws Exception {
            final UacElevatedEnvSnapshot snapshot = new UacElevatedEnvSnapshot();
            snapshot.isElevated = WindowsUtils.isElevated();
            // Step 1: Set APWORK_RUNAS_HELPER_ENV_PROBE = ELEVATED_ADMIN_UAC_xxx in elevated admin JVM
            RunAsHelperSecurityTestSupport.setProcessEnvironmentVariable(RunAsHelperSecurityTestSupport.ENV_PROBE_NAME, marker);
            // Step 2: Capture elevated admin env map — host expects marker present
            snapshot.elevatedAdminEnv = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvironment();
            // Step 3: Launch de-elevated session-owner child via runUACElevated; child dumps full env
            final ProcessOutput sessionOwnerOut = RunAsHelper.runUACElevated(RunAsHelperSecurityTestSupport.powershellEnvDumpAllArgv(), RunAsLaunchOptions.builder().waitFor(true).build());
            // Step 4: Capture session-owner env map — host expects marker absent or not equal
            snapshot.sessionOwnerProbe = RunAsHelperSecurityTestSupport.captureEnvMapProbe(sessionOwnerOut);
            return snapshot;
        }
    }
}
