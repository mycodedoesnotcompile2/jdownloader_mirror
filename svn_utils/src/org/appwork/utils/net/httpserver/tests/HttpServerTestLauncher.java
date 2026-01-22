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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver.tests;

import java.util.ArrayList;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.IDETestRunner;
import org.appwork.testframework.TestInterface;

/**
 * Launcher class that executes all HTTP server tests sequentially.
 *
 * <p>
 * This class runs all HTTP server test classes in a predefined order:
 * </p>
 * <ol>
 * <li>HttpServerLocalhostBindingTest - Tests localhost binding</li>
 * <li>HttpServerRequestSizeLimitsTest - Tests request size limits</li>
 * <li>HttpServerHeaderValidationTest - Tests header validation</li>
 * <li>HttpServerMethodValidationTest - Tests HTTP method validation</li>
 * <li>HttpServerCorsTest - Tests CORS handling</li>
 * <li>HttpServerSecurityHeadersTest - Tests response security headers</li>
 * <li>HttpServerAttackScenariosTest - Tests attack scenarios</li>
 * </ol>
 *
 * <p>
 * Usage:
 * </p>
 *
 * <pre>
 * java org.appwork.remoteapi.tests.HttpServerTestLauncher
 * </pre>
 *
 * @author AppWork
 */
public class HttpServerTestLauncher {

    /**
     * Main method that executes all HTTP server tests sequentially.
     *
     * @param args
     *            Command line arguments (not used)
     */
    public static void main(final String[] args) {
        final List<Class<? extends TestInterface>> testClasses = new ArrayList<Class<? extends TestInterface>>();

        // Add all HTTP server test classes in execution order
        testClasses.add(HttpServerLocalhostBindingTest.class);
        testClasses.add(HttpServerRequestSizeLimitsTest.class);
        testClasses.add(HttpServerHeaderValidationTest.class);
        testClasses.add(HttpServerMethodValidationTest.class);

        testClasses.add(HttpServerCorsTest.class);
        testClasses.add(HttpServerSecurityHeadersTest.class);
        testClasses.add(HttpServerAttackScenariosTest.class);
        testClasses.add(HttpServerConnectionTimeoutsTest.class);
        testClasses.add(HttpServerRemoteAPITest.class);
        testClasses.add(HttpServerHeaderVerificationTest.class);

        LogV3.info("========================================");
        LogV3.info("HTTP Server Test Suite Launcher");
        LogV3.info("========================================");
        LogV3.info("Starting execution of " + testClasses.size() + " test classes...");
        LogV3.info("");

        final List<String> failedTests = new ArrayList<String>();
        final List<String> passedTests = new ArrayList<String>();

        int testNumber = 1;
        for (final Class<? extends TestInterface> testClass : testClasses) {
            final String testName = testClass.getSimpleName();
            LogV3.info("[" + testNumber + "/" + testClasses.size() + "] Running: " + testName);
            LogV3.info("----------------------------------------");

            try {
                IDETestRunner.run(testClass);
                passedTests.add(testName);
                LogV3.info("✓ " + testName + " PASSED");
            } catch (final Throwable e) {
                failedTests.add(testName);
                LogV3.warning("✗ " + testName + " FAILED: " + e.getMessage());
                LogV3.log(e);
            }

            LogV3.info("");
            testNumber++;
        }

        // Print summary
        LogV3.info("========================================");
        LogV3.info("Test Suite Summary");
        LogV3.info("========================================");
        LogV3.info("Total tests: " + testClasses.size());
        LogV3.info("Passed: " + passedTests.size());
        LogV3.info("Failed: " + failedTests.size());
        LogV3.info("");

        if (!passedTests.isEmpty()) {
            LogV3.info("Passed tests:");
            for (final String testName : passedTests) {
                LogV3.info("  ✓ " + testName);
            }
            LogV3.info("");
        }

        if (!failedTests.isEmpty()) {
            LogV3.warning("Failed tests:");
            for (final String testName : failedTests) {
                LogV3.warning("  ✗ " + testName);
            }
            LogV3.info("");
            System.exit(1); // Exit with error code if any test failed
        } else {
            LogV3.info("All tests passed successfully!");
            System.exit(0); // Exit with success code
        }
    }
}
