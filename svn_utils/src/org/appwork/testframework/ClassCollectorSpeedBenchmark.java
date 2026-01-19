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

import java.util.Map;

/**
 * Simple standalone benchmark for ClassCollector vs ClassCollector2
 * Can be run directly with: java ClassCollectorSpeedBenchmark
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class ClassCollectorSpeedBenchmark {

    private static final String[] TEST_CLASSES = {
            "org.appwork.testframework.IDETestRunner",
            "org.appwork.utils.net.httpserver.HttpConnection",
            "org.appwork.storage.JSonStorage",
            "org.appwork.utils.Application",
            "org.appwork.loggingv3.LogV3"
    };

    public static void main(String[] args) throws Exception {
        System.out.println("=====================================");
        System.out.println("ClassCollector Performance Benchmark");
        System.out.println("=====================================");
        System.out.println();

        // Warm-up JVM
        System.out.println("Warming up JVM...");
        for (int i = 0; i < 3; i++) {
            try {
                new ClassCollector().getClasses("org.appwork.testframework.ClassCollector", true);
                new ClassCollector2().getClasses("org.appwork.testframework.ClassCollector2", true);
            } catch (Exception e) {
                // Ignore
            }
        }
        Thread.sleep(100);
        System.out.println();

        // Test 1: First run comparison
        System.out.println("=== Test 1: Cold Start (Single Class) ===");
        testColdStart();
        System.out.println();

        // Test 2: Multiple iterations with cache
        System.out.println("=== Test 2: Cache Benefit (5 Iterations) ===");
        testCacheBenefit();
        System.out.println();

        // Test 3: Real-world scenario
        System.out.println("=== Test 3: Real-World Scenario (Multiple Classes) ===");
        testRealWorld();
        System.out.println();

        System.out.println("=== Benchmark Complete ===");
    }

    private static void testColdStart() throws Exception {
        String testClass = "org.appwork.testframework.IDETestRunner";

        // Clear cache for fair comparison
        ClassCollector2.clearCache();
        System.gc();
        Thread.sleep(100);

        // Test old
        long startOld = System.nanoTime();
        ClassCollector collectorOld = new ClassCollector();
        Map<String, String> resultOld = collectorOld.getClasses(testClass, true);
        long durationOld = System.nanoTime() - startOld;

        // Clear cache for new
        ClassCollector2.clearCache();
        System.gc();
        Thread.sleep(100);

        // Test new
        long startNew = System.nanoTime();
        ClassCollector2 collectorNew = new ClassCollector2();
        Map<String, String> resultNew = collectorNew.getClasses(testClass, true);
        long durationNew = System.nanoTime() - startNew;

        System.out.printf("ClassCollector (old):  %4d classes in %7.2f ms%n", resultOld.size(), durationOld / 1_000_000.0);
        System.out.printf("ClassCollector2 (new): %4d classes in %7.2f ms%n", resultNew.size(), durationNew / 1_000_000.0);
        System.out.printf("Speedup: %.2fx %s%n",
                (double) durationOld / durationNew,
                durationNew < durationOld ? "✓ NEW FASTER" : "✗ OLD FASTER");

        if (resultOld.size() == resultNew.size()) {
            System.out.println("✓ Both found same number of classes");
        } else {
            System.out.printf("⚠ Different results: old=%d, new=%d%n", resultOld.size(), resultNew.size());
        }
    }

    private static void testCacheBenefit() throws Exception {
        int iterations = 5;
        String testClass = "org.appwork.testframework.IDETestRunner";

        System.out.println("Testing " + iterations + " iterations to show cache benefit...");
        System.out.println();

        // Clear cache
        ClassCollector2.clearCache();

        // Test old (no cache)
        long totalOld = 0;
        for (int i = 0; i < iterations; i++) {
            long start = System.nanoTime();
            new ClassCollector().getClasses(testClass, true);
            long duration = System.nanoTime() - start;
            totalOld += duration;
            System.out.printf("Old - Run %d: %6.2f ms%n", i + 1, duration / 1_000_000.0);
        }

        System.out.println();

        // Test new (with cache building up)
        ClassCollector2.clearCache();
        long totalNew = 0;
        long[] timesNew = new long[iterations];
        for (int i = 0; i < iterations; i++) {
            long start = System.nanoTime();
            new ClassCollector2().getClasses(testClass, true);
            long duration = System.nanoTime() - start;
            totalNew += duration;
            timesNew[i] = duration;
            System.out.printf("New - Run %d: %6.2f ms%n", i + 1, duration / 1_000_000.0);
        }

        System.out.println();
        System.out.printf("Average Old: %6.2f ms (consistent, no cache)%n", totalOld / iterations / 1_000_000.0);
        System.out.printf("Average New: %6.2f ms (benefits from cache)%n", totalNew / iterations / 1_000_000.0);
        System.out.printf("Overall Speedup: %.2fx%n", (double) totalOld / totalNew);
        System.out.println();

        // Show cache improvement
        if (timesNew[0] > timesNew[iterations - 1]) {
            double cacheSpeedup = (double) timesNew[0] / timesNew[iterations - 1];
            System.out.printf("Cache Benefit: Run 1: %.2f ms → Run %d: %.2f ms (%.2fx faster!)%n",
                    timesNew[0] / 1_000_000.0,
                    iterations,
                    timesNew[iterations - 1] / 1_000_000.0,
                    cacheSpeedup);
        }

        System.out.println();
        System.out.println(ClassCollector2.getCacheStats());
    }

    private static void testRealWorld() throws Exception {
        System.out.println("Simulating real-world usage: scanning " + TEST_CLASSES.length + " different test classes...");
        System.out.println("(Classes share many common dependencies)");
        System.out.println();

        // Test old
        long startOld = System.nanoTime();
        int totalClassesOld = 0;
        for (String className : TEST_CLASSES) {
            try {
                Map<String, String> result = new ClassCollector().getClasses(className, true);
                totalClassesOld += result.size();
            } catch (Exception e) {
                System.out.println("Skip " + className + ": " + e.getMessage());
            }
        }
        long durationOld = System.nanoTime() - startOld;

        // Test new
        ClassCollector2.clearCache();
        long startNew = System.nanoTime();
        int totalClassesNew = 0;
        for (String className : TEST_CLASSES) {
            try {
                Map<String, String> result = new ClassCollector2().getClasses(className, true);
                totalClassesNew += result.size();
            } catch (Exception e) {
                System.out.println("Skip " + className + ": " + e.getMessage());
            }
        }
        long durationNew = System.nanoTime() - startNew;

        System.out.printf("ClassCollector (old):  scanned %4d total classes in %7.2f ms%n",
                totalClassesOld, durationOld / 1_000_000.0);
        System.out.printf("ClassCollector2 (new): scanned %4d total classes in %7.2f ms%n",
                totalClassesNew, durationNew / 1_000_000.0);
        System.out.printf("Speedup: %.2fx %s%n",
                (double) durationOld / durationNew,
                durationNew < durationOld ? "✓ NEW FASTER" : "✗ OLD FASTER");
        System.out.println();
        System.out.println(ClassCollector2.getCacheStats());

        // Calculate efficiency
        long uniqueClassesScanned = ClassCollector2.getCacheSize();
        System.out.println();
        System.out.printf("Efficiency: Only %d unique classes scanned (%.1f%% cache hit rate on duplicates)%n",
                uniqueClassesScanned,
                (1.0 - (double) uniqueClassesScanned / totalClassesNew) * 100);
    }
}
