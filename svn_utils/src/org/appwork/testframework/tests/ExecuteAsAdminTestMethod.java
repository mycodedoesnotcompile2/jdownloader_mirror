/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

/**
 * Methods invoked via {@link org.appwork.testframework.JREExecuter} for testing.
 * Used by {@link TestJREProviderTest#testExecuteAsAdminBasic} and
 * {@link TestJREProviderTest#testExecuteAsAdminStdoutStderrExceptionPassThrough}.
 */
public class ExecuteAsAdminTestMethod {

    /**
     * Succeeds without side effects. Used to verify executeAsAdmin runs and returns.
     */
    public static void succeed() {
        // no-op
    }

    /**
     * Writes a known line to stdout, a known line to stderr, then throws an exception with a known message.
     * Used to verify stdout, stderr and exception are passed through when run via executeAsAdmin.
     */
    public static void printAndThrow() {
        System.out.println("EXECUTE_AS_ADMIN_STDOUT_LINE");
        System.out.flush();
        System.err.println("EXECUTE_AS_ADMIN_STDERR_LINE");
        System.err.flush();
        throw new RuntimeException("EXECUTE_AS_ADMIN_EXCEPTION_MSG");
    }
}
