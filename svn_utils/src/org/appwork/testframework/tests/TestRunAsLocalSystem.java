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
package org.appwork.testframework.tests;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.TestTag;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.LogCallback;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;
import org.appwork.utils.os.WindowsUtils;

/**
 * AWTest test class for {@link AdminExecuter#runAsLocalSystem}. Verifies that code is executed as NT AUTHORITY\SYSTEM (LocalSystem).
 * Prefers {@link AdminExecuter#runAsLocalSystem(ElevatedTestTask, TypeRef, ProcessOptions)} and
 * {@link AdminExecuter#runAsLocalSystem(Class, String, TypeRef, Object...)}; uses command-line only when necessary (e.g. LogCallback test).
 * Windows-only.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsLocalSystem extends AWTest implements Serializable {
    private static final long serialVersionUID = 1L;

    public static void main(String[] args) {
        run();
    }

    @Override
    public Set<TestTag> getTags() {
        return EnumSet.of(TestTag.UAC);
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsLocalSystem: Windows only, skipped.");
            return;
        }
        testRunAsLocalSystemExitCode();
        testRunAsLocalSystemStdout();
        testRunAsLocalSystemWhoami();
        testRunAsLocalSystemSid();
        testRunAsLocalSystemTask();
        testRunAsLocalSystemStaticMethodVarargs();
        testRunAsLocalSystemStaticMethodExplicitTypes();
        testRunAsLocalSystemLogCallback();
        testRunAsLocalSystemAlreadyInContext();
    }

    /**
     * runAsLocalSystem(ElevatedTestTask) runs cmd exit 0 and returns exit code via task result.
     */
    private void testRunAsLocalSystemExitCode() throws Exception {
        Integer exitCode = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                ProcessOutput out = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "exit", "0");
                return Integer.valueOf(out != null ? out.getExitCode() : -1);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);
        assertTrue(exitCode != null, "ProcessOutput must not be null");
        assertEquals(0, exitCode.intValue(), "exit 0 should return 0");
    }

    /**
     * runAsLocalSystem(ElevatedTestTask) captures stdout via task return value.
     */
    private void testRunAsLocalSystemStdout() throws Exception {
        String stdout = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                ProcessOutput out = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "echo", "RUN_AS_LOCAL_SYSTEM_MARKER");
                return out != null && out.getStdOutString() != null ? out.getStdOutString() : "";
            }
        }, TypeRef.STRING, ProcessOptions.DEFAULT);
        assertTrue(stdout != null && stdout.contains("RUN_AS_LOCAL_SYSTEM_MARKER"), "stdout should contain marker, got: " + stdout);
    }

    /**
     * runAsLocalSystem(ElevatedTestTask) runs whoami as SYSTEM; task returns stdout.
     */
    private void testRunAsLocalSystemWhoami() throws Exception {
        String stdout = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                ProcessOutput out = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "whoami");
                return out != null && out.getStdOutString() != null ? out.getStdOutString() : "";
            }
        }, TypeRef.STRING, ProcessOptions.DEFAULT);
        assertTrue(stdout != null, "whoami stdout must not be null");
        String lower = stdout.trim().toLowerCase();
        boolean isSystemAccount = lower.contains("system") && (lower.contains("nt authority") || lower.contains("nt-autorit"));
        assertTrue(isSystemAccount, "whoami must show NT AUTHORITY\\SYSTEM (or NT-Autorität\\system), got: " + stdout);
    }

    /**
     * runAsLocalSystem(ElevatedTestTask) runs whoami /user; task returns stdout (must contain S-1-5-18).
     */
    private void testRunAsLocalSystemSid() throws Exception {
        String stdout = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                ProcessOutput out = ProcessBuilderFactory.runCommand("whoami", "/user");
                return out != null && out.getStdOutString() != null ? out.getStdOutString() : "";
            }
        }, TypeRef.STRING, ProcessOptions.DEFAULT);
        assertTrue(stdout != null && stdout.contains("S-1-5-18"), "whoami /user must show SID S-1-5-18 (Local System), got: " + stdout);
    }

    /**
     * runAsLocalSystem(AdminTestTask) runs the task as NT AUTHORITY\SYSTEM and returns the task result.
     */
    private void testRunAsLocalSystemTask() throws Exception {
        Object result = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                ProcessOutput whoami = ProcessBuilderFactory.runCommand("whoami");
                assertTrue(whoami != null && whoami.getStdOutString() != null, "whoami must return stdout");
                String out = whoami.getStdOutString().trim().toLowerCase();
                assertTrue(out.contains("system") && (out.contains("nt authority") || out.contains("nt-autorit")), "task must run as SYSTEM, got: " + whoami.getStdOutString());
                return Integer.valueOf(42);
            }
        }, TypeRef.OBJECT, ProcessOptions.DEFAULT);
        assertTrue(result instanceof Integer, "task must return Integer result");
        assertEquals(42, ((Integer) result).intValue(), "task should return 42");
    }

    /**
     * runAsLocalSystem(Class, String, Object...) runs a static method as SYSTEM; parameter types are inferred from varargs.
     */
    private void testRunAsLocalSystemStaticMethodVarargs() throws Exception {
        String result = AdminExecuter.runAsLocalSystem(RunAsStaticMethodTarget.class, "marker", TypeRef.STRING);
        assertEquals("RUN_AS_STATIC_METHOD_MARKER", result, "marker() should return RUN_AS_STATIC_METHOD_MARKER");
    }

    /**
     * runAsLocalSystem(Class, String, Class[], Object[]) runs a static method as SYSTEM with explicit parameter types.
     */
    private void testRunAsLocalSystemStaticMethodExplicitTypes() throws Exception {
        String result = AdminExecuter.runAsLocalSystem(RunAsStaticMethodTarget.class, "echo", new Class<?>[] { String.class }, new Object[] { "HELLO_SYSTEM" }, TypeRef.STRING);
        assertEquals("HELLO_SYSTEM", result, "echo(HELLO_SYSTEM) should return HELLO_SYSTEM");
    }

    /**
     * runAsLocalSystem(File, String[], ProcessOptions) with LogCallback: command-line only when necessary to test callback streaming.
     */
    private void testRunAsLocalSystemLogCallback() throws Exception {
        final List<String> stdoutLines = Collections.synchronizedList(new ArrayList<String>());
        final List<String> stderrLines = Collections.synchronizedList(new ArrayList<String>());
        LogCallback callback = new LogCallback() {
            @Override
            public void onStdOut(String line) {
                stdoutLines.add(line);
            }

            @Override
            public void onStdErr(String line) {
                stderrLines.add(line);
            }
        };
        ProcessOptions options = ProcessOptions.builder().logCallback(callback).build();
        stdoutLines.clear();
        stderrLines.clear();
        ProcessOutput out = AdminExecuter.runAsLocalSystem(null, new String[] { "cmd.exe", "/c", "echo", "RUN_AS_LOCAL_SYSTEM_LOG_CB_MARKER" }, options);
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(stdoutLines.size() >= 1, "LogCallback.onStdOut should be called at least once, got: " + stdoutLines.size());
        boolean hasMarker = false;
        for (String line : stdoutLines) {
            if (line != null && line.contains("RUN_AS_LOCAL_SYSTEM_LOG_CB_MARKER")) {
                hasMarker = true;
                break;
            }
        }
        assertTrue(hasMarker, "LogCallback should receive line containing RUN_AS_LOCAL_SYSTEM_LOG_CB_MARKER, got: " + stdoutLines);
        logInfoAnyway("  OK: runAsLocalSystem LogCallback received stdout lines");
    }

    /**
     * When already running as LocalSystem, a nested runAsLocalSystem call runs in-process (no helper/schtasks). Outer task runs as SYSTEM via
     * helper; inside it runAsLocalSystem is called again and must execute directly and return the inner task result.
     */
    private void testRunAsLocalSystemAlreadyInContext() throws Exception {
        Integer result = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isRunningAsLocalSystem(), "This task must run as LocalSystem (in helper/schtasks)");
                String nested = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
                    private static final long serialVersionUID = 1L;

                    @Override
                    public Serializable run() throws Exception {
                        return "ALREADY_LOCAL_SYSTEM_IN_PROCESS";
                    }
                }, TypeRef.STRING, ProcessOptions.DEFAULT);
                assertTrue("ALREADY_LOCAL_SYSTEM_IN_PROCESS".equals(nested), "Nested runAsLocalSystem must run in-process and return marker, got: " + nested);
                return Integer.valueOf(0);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);
        assertTrue(result != null && result.intValue() == 0, "outer task should return 0");
    }
}
