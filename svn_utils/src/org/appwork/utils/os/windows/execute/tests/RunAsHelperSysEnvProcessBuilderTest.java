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

import java.util.HashMap;

import org.appwork.JNAHelper;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.crypto.Crypto;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessOutput;

/**
 * POSITIVE control: documents expected env behaviour for the {@link java.lang.ProcessBuilder} path of
 * {@link RunAsHelper#runInOwnerSession} when host JVM is same interactive user, not elevated, with {@link RunAsLaunchOptions#isWaitFor()}
 * {@code true}. Unlike token launch in {@link RunAsHelperSysEnvTest}, the child inherits the host JVM process environment.
 * <p>
 * Probes return full env maps; host evaluates {@link RunAsHelperSecurityTestSupport#ENV_PROBE_NAME}.
 *
 * <pre>
 * Flow
 *  |
 *  +-- Skip if not Windows/JNA, host elevated, Local System, or no WTS session
 *  |
 *  +-- Set APWORK_RUNAS_HELPER_ENV_PROBE = HOST_JVM_xxx in host JVM
 *  +-- Capture host JVM env map — We expect env[APWORK_RUNAS_HELPER_ENV_PROBE] == HOST_JVM_xxx
 *  |
 *  +-- RunAsHelper.runInOwnerSession (ProcessBuilder path, same user) with env dump-all
 *  +-- We expect session-owner env[APWORK_RUNAS_HELPER_ENV_PROBE] == HOST_JVM_xxx (inherited)
 * </pre>
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperSysEnvProcessBuilderTest extends AWTest {

    public static void main(String[] args) {
        run();
    }

    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public void runTest() throws Exception {
        // Step 1: Skip if prerequisites not met (ProcessBuilder path only applies to non-elevated interactive user)
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperSysEnvProcessBuilderTest: Windows + JNA required, skipped.");
            return;
        }
        if (WindowsUtils.isElevated()) {
            logInfoAnyway("RunAsHelperSysEnvProcessBuilderTest: skip (host elevated; ProcessBuilder path not used).");
            return;
        }
        if (WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("RunAsHelperSysEnvProcessBuilderTest: skip (LocalSystem host).");
            return;
        }
        final int sessionId = WindowsUtils.getCurrentProcessSessionId();
        if (sessionId < 0) {
            logInfoAnyway("RunAsHelperSysEnvProcessBuilderTest: no WTS session, skipped.");
            return;
        }
        final String marker = "HOST_JVM_" + Crypto.generateRandomString(8, "abcdefghijklmnopqrstuvwxyz0123456789");
        // Step 2: Set APWORK_RUNAS_HELPER_ENV_PROBE = HOST_JVM_xxx in host JVM
        RunAsHelperSecurityTestSupport.setProcessEnvironmentVariable(RunAsHelperSecurityTestSupport.ENV_PROBE_NAME, marker);
        // Step 3: Capture host JVM env map — We expect env[APWORK_RUNAS_HELPER_ENV_PROBE] == HOST_JVM_xxx
        final HashMap<String, String> hostJvmEnv = RunAsHelperSecurityTestSupport.captureCurrentProcessEnvironment();
        // Host JVM must expose the planted marker before the ProcessBuilder child launch.
        assertEnvMarkerEquals("hostJvm", marker, hostJvmEnv);
        // Step 4: Launch session-owner child via ProcessBuilder path; child dumps full env map
        final ProcessOutput sessionOwnerOut = RunAsHelper.runInOwnerSession(RunAsHelperSecurityTestSupport.powershellEnvDumpAllArgv(), RunAsLaunchOptions.DEFAULT);
        final RunAsHelperSecurityTestSupport.EnvProbeRun sessionOwnerProbe = RunAsHelperSecurityTestSupport.captureEnvMapProbe(sessionOwnerOut);
        // Child env dump must succeed so inheritance can be verified.
        assertEquals(0, sessionOwnerProbe.exitCode, "sessionOwner probe exit, stderr=" + sessionOwnerProbe.stderr);
        // POSITIVE: ProcessBuilder same-user path must inherit the host JVM probe marker.
        assertEnvMarkerEquals("sessionOwner-processbuilder", marker, sessionOwnerProbe.env);
        logInfoAnyway("RunAsHelperSysEnvProcessBuilderTest: ProcessBuilder path inherited marker (expected for same-user launch).");
    }

    private void assertEnvMarkerEquals(final String label, final String expectedMarker, final HashMap<String, String> env) throws Exception {
        // Env map from the probe dump is required to read the marker.
        assertTrue(env != null, label + ": env map null");
        final String value = RunAsHelperSecurityTestSupport.envValue(env, RunAsHelperSecurityTestSupport.ENV_PROBE_NAME);
        // Probe variable must be present when we expect the planted marker.
        assertTrue(value != null, label + ": expected " + RunAsHelperSecurityTestSupport.ENV_PROBE_NAME + "==" + expectedMarker + ", got absent");
        // Marker value must exactly match what the host/child process set or inherited.
        assertEquals(expectedMarker, value, label + ": marker value");
    }
}
