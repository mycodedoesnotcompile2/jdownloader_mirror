/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.Serializable;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import org.appwork.testframework.executer.ProcessOptions;

/**
 * AWTest test class for {@link AdminExecuter#runAsAdmin(ElevatedTestTask, TypeRef)}. The task is serialized and executed in the elevated
 * helper process (runs with elevated privileges; use ProcessBuilderFactory for subprocess commands). Windows-only.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsAdminTask extends AWTest implements Serializable {
    private static final long serialVersionUID = 1L;

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsAdminTask: Windows only, skipped.");
            return;
        }
        testRunAsAdminTaskReceivesRunner();
        testRunAsAdminTaskMultipleCommands();
        testRunAsAdminStaticMethodVarargs();
        testRunAsAdminStaticMethodExplicitTypes();
        testRunAsAdminAlreadyInContext();
    }

    /**
     * runAsAdmin(AdminTestTask) serializes the task and runs it in the elevated helper. The task runs elevated and returns a result.
     */
    private void testRunAsAdminTaskReceivesRunner() throws Exception {
        Integer result = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                System.out.println("here I am (stdout from elevated task)");
                System.err.println("(stderr from elevated task)");
                LogV3.info("Here I am: " + WindowsUtils.isElevated());
                assertTrue(WindowsUtils.isElevated(), "Must run as admin (task runs in elevated helper)");
                ProcessOutput sessionOut = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "net session >nul 2>&1");
                assertTrue(sessionOut != null && sessionOut.getExitCode() == 0, "net session must succeed as admin");
                ProcessOutput exitOut = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "exit", "0");
                assertTrue(exitOut != null, "runCommand must return ProcessOutput");
                return Integer.valueOf(exitOut.getExitCode());
            }
        }, TypeRef.INT);
        assertTrue(result instanceof Integer, "task must return Integer result");
        assertEquals(0, result.intValue(), "exit 0 should return 0");
    }

    /**
     * runAsAdmin(AdminTestTask) allows multiple commands in one task (same helper, one UAC).
     */
    private void testRunAsAdminTaskMultipleCommands() throws Exception {
        Integer result = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isElevated(), "Must run as admin (helper elevated)");
                ProcessOutput o1 = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "echo", "FIRST");
                ProcessOutput o2 = ProcessBuilderFactory.runCommand("cmd.exe", "/c", "echo", "SECOND");
                assertTrue(o1 != null && o1.getStdOutString() != null && o1.getStdOutString().contains("FIRST"), "first command stdout");
                assertTrue(o2 != null && o2.getStdOutString() != null && o2.getStdOutString().contains("SECOND"), "second command stdout");
                return Integer.valueOf(2);
            }
        }, TypeRef.INT);
        assertEquals(2, result.intValue(), "task should have run two commands");
    }

    /**
     * runAsAdmin(Class, String, Object...) runs a static method in the elevated helper; parameter types are inferred from varargs.
     */
    private void testRunAsAdminStaticMethodVarargs() throws Exception {
        Object result = AdminExecuter.runAsAdmin(RunAsStaticMethodTarget.class, "add", TypeRef.OBJECT, Integer.valueOf(3), Integer.valueOf(7));
        assertTrue(result instanceof Integer, "add(3,7) must return Integer");
        assertEquals(10, ((Integer) result).intValue(), "add(3,7) should return 10");
    }

    /**
     * runAsAdmin(Class, String, Class[], Object[]) runs a static method with explicit parameter types.
     */
    private void testRunAsAdminStaticMethodExplicitTypes() throws Exception {
        Object result = AdminExecuter.runAsAdmin(RunAsStaticMethodTarget.class, "echo", new Class<?>[] { String.class }, new Object[] { "HELLO_ADMIN" }, TypeRef.OBJECT);
        assertTrue(result instanceof String, "echo must return String");
        assertEquals("HELLO_ADMIN", result, "echo(HELLO_ADMIN) should return HELLO_ADMIN");
    }

    /**
     * When already elevated, a nested runAsAdmin call runs in-process (no helper). Outer task runs elevated in helper; inside it runAsAdmin is
     * called again and must execute directly and return the inner task result.
     */
    private void testRunAsAdminAlreadyInContext() throws Exception {
        Integer result = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isElevated(), "This task must run elevated (in helper)");
                String nested = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
                    private static final long serialVersionUID = 1L;

                    @Override
                    public Serializable run() throws Exception {
                        return "ALREADY_ELEVATED_IN_PROCESS";
                    }
                }, TypeRef.STRING, ProcessOptions.DEFAULT);
                assertTrue("ALREADY_ELEVATED_IN_PROCESS".equals(nested), "Nested runAsAdmin must run in-process and return marker, got: " + nested);
                return Integer.valueOf(0);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);
        assertTrue(result != null && result.intValue() == 0, "outer task should return 0");
    }
}
