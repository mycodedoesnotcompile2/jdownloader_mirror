/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.testframework;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Performance comparison test for ClassCollector vs ClassCollector2
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class ClassCollectorSpeedTest extends AWTest {

    private static final String[] TEST_CLASSES = {
            "org.appwork.utils.net.httpserver.HttpConnection",
            "org.appwork.storage.JSonStorage",
            "org.appwork.utils.Application",
            "org.appwork.loggingv3.LogV3",
            "org.appwork.testframework.IDETestRunner"
    };

    @Override
    public void runTest() throws Exception {
        logInfoAnyway("=== ClassCollector Performance Test ===");
        logInfoAnyway("");

        // Warm-up: Load classes into JVM
        logInfoAnyway("Warming up JVM...");
        for (String testClass : TEST_CLASSES) {
            try {
                Class.forName(testClass);
            } catch (ClassNotFoundException e) {
                // Ignore - class might not exist
            }
        }

        logInfoAnyway("");
        logInfoAnyway("--- Test 1: Single Run (Cold Start) ---");
        testSingleRun();

        logInfoAnyway("");
        logInfoAnyway("--- Test 2: Multiple Runs (Cache Effect) ---");
        testMultipleRuns();

        logInfoAnyway("");
        logInfoAnyway("--- Test 3: Large Test (All Test Classes) ---");
        testLargeRun();

        logInfoAnyway("");
        logInfoAnyway("=== Test Complete ===");
    }

    private void testSingleRun() throws IOException {
        String testClass = "org.appwork.testframework.IDETestRunner";

        // Test ClassCollector (old)
        long startOld = System.nanoTime();
        ClassCollector collectorOld = new ClassCollector();
        Map<String, String> resultOld = collectorOld.getClasses(testClass, true);
        long durationOld = System.nanoTime() - startOld;

        // Test ClassCollector2 (new) - clear cache first for fair comparison
        ClassCollector2.clearCache();
        long startNew = System.nanoTime();
        ClassCollector2 collectorNew = new ClassCollector2();
        Map<String, String> resultNew = collectorNew.getClasses(testClass, true);
        long durationNew = System.nanoTime() - startNew;

        // Results
        logInfoAnyway(String.format("ClassCollector (old):  %d classes in %6.2f ms", resultOld.size(), durationOld / 1_000_000.0));
        logInfoAnyway(String.format("ClassCollector2 (new): %d classes in %6.2f ms", resultNew.size(), durationNew / 1_000_000.0));
        logInfoAnyway(String.format("Speedup: %.2fx %s", (double) durationOld / durationNew, durationNew < durationOld ? "(NEW FASTER)" : "(OLD FASTER)"));

        // Verify results match
        if (resultOld.size() != resultNew.size()) {
            logInfoAnyway("WARNING: Different number of classes found!");
        } else {
            logInfoAnyway("✓ Both collectors found same number of classes");
        }
    }

    private void testMultipleRuns() throws IOException {
        int runs = 5;
        List<Long> timesOld = new ArrayList<Long>();
        List<Long> timesNew = new ArrayList<Long>();

        logInfoAnyway("Running " + runs + " iterations on " + TEST_CLASSES.length + " test classes...");
        logInfoAnyway("");

        // Clear cache for ClassCollector2
        ClassCollector2.clearCache();

        for (int i = 0; i < runs; i++) {
            logInfoAnyway("Run " + (i + 1) + "/" + runs + ":");

            // Test ClassCollector (old)
            long startOld = System.nanoTime();
            for (String testClass : TEST_CLASSES) {
                try {
                    ClassCollector collectorOld = new ClassCollector();
                    collectorOld.getClasses(testClass, true);
                } catch (Exception e) {
                    // Ignore missing classes
                }
            }
            long durationOld = System.nanoTime() - startOld;
            timesOld.add(durationOld);

            // Test ClassCollector2 (new) - cache persists between runs!
            long startNew = System.nanoTime();
            for (String testClass : TEST_CLASSES) {
                try {
                    ClassCollector2 collectorNew = new ClassCollector2();
                    collectorNew.getClasses(testClass, true);
                } catch (Exception e) {
                    // Ignore missing classes
                }
            }
            long durationNew = System.nanoTime() - startNew;
            timesNew.add(durationNew);

            logInfoAnyway(String.format("  Old: %6.2f ms | New: %6.2f ms | Speedup: %.2fx",
                    durationOld / 1_000_000.0,
                    durationNew / 1_000_000.0,
                    (double) durationOld / durationNew));
        }

        // Calculate averages
        long avgOld = timesOld.stream().mapToLong(Long::longValue).sum() / runs;
        long avgNew = timesNew.stream().mapToLong(Long::longValue).sum() / runs;

        logInfoAnyway("");
        logInfoAnyway(String.format("Average Old: %6.2f ms", avgOld / 1_000_000.0));
        logInfoAnyway(String.format("Average New: %6.2f ms", avgNew / 1_000_000.0));
        logInfoAnyway(String.format("Overall Speedup: %.2fx", (double) avgOld / avgNew));
        logInfoAnyway("");
        logInfoAnyway(ClassCollector2.getCacheStats());

        // Show cache benefit
        if (timesNew.size() > 1) {
            long firstRun = timesNew.get(0);
            long lastRun = timesNew.get(timesNew.size() - 1);
            logInfoAnyway(String.format("Cache Benefit: First run: %.2f ms, Last run: %.2f ms (%.2fx faster)",
                    firstRun / 1_000_000.0,
                    lastRun / 1_000_000.0,
                    (double) firstRun / lastRun));
        }
    }

    private void testLargeRun() throws IOException {
        logInfoAnyway("Scanning all test framework classes...");

        String[] largeTestSet = {
                "org.appwork.testframework.AWTest",
                "org.appwork.testframework.IDETestRunner",
                "org.appwork.testframework.ClassCollector",
                "org.appwork.testframework.ClassCollector2",
                "org.appwork.utils.net.httpserver.HttpConnection",
                "org.appwork.storage.JSonStorage",
                "org.appwork.storage.simplejson.JSonFactory",
                "org.appwork.utils.Application",
                "org.appwork.loggingv3.LogV3",
                "org.appwork.utils.Hash"
        };

        // Test ClassCollector (old)
        long startOld = System.nanoTime();
        int totalClassesOld = 0;
        for (String testClass : largeTestSet) {
            try {
                ClassCollector collectorOld = new ClassCollector();
                Map<String, String> result = collectorOld.getClasses(testClass, true);
                totalClassesOld += result.size();
            } catch (Exception e) {
                // Ignore
            }
        }
        long durationOld = System.nanoTime() - startOld;

        // Test ClassCollector2 (new) - cache already warmed up from previous tests!
        long startNew = System.nanoTime();
        int totalClassesNew = 0;
        for (String testClass : largeTestSet) {
            try {
                ClassCollector2 collectorNew = new ClassCollector2();
                Map<String, String> result = collectorNew.getClasses(testClass, true);
                totalClassesNew += result.size();
            } catch (Exception e) {
                // Ignore
            }
        }
        long durationNew = System.nanoTime() - startNew;

        logInfoAnyway(String.format("ClassCollector (old):  %d total classes in %6.2f ms", totalClassesOld, durationOld / 1_000_000.0));
        logInfoAnyway(String.format("ClassCollector2 (new): %d total classes in %6.2f ms", totalClassesNew, durationNew / 1_000_000.0));
        logInfoAnyway(String.format("Speedup: %.2fx", (double) durationOld / durationNew));
        logInfoAnyway("");
        logInfoAnyway(ClassCollector2.getCacheStats());
    }

    public static void main(String[] args) {
        try {
            ClassCollectorSpeedTest test = new ClassCollectorSpeedTest();
            test.runTest();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
