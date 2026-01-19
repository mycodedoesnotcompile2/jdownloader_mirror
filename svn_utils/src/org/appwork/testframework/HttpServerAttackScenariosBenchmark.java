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
 * Benchmark: Scans HttpServerAttackScenariosTest 100 times with both collectors
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class HttpServerAttackScenariosBenchmark {

    private static final String TEST_CLASS = "org.appwork.utils.net.httpserver.tests.HttpServerAttackScenariosTest";
    private static final int ITERATIONS = 100;

    public static void main(String[] args) {
        System.out.println("=================================================================");
        System.out.println("Performance Benchmark: HttpServerAttackScenariosTest");
        System.out.println("=================================================================");
        System.out.println("Scanning class " + ITERATIONS + " times with each collector");
        System.out.println();

        try {
            // Warm-up JVM
            System.out.println("Warming up JVM...");
            for (int i = 0; i < 5; i++) {
                try {
                    new ClassCollector().getClasses(TEST_CLASS, true);
                    new ClassCollector2().getClasses(TEST_CLASS, true);
                } catch (Exception e) {
                    // Ignore
                }
            }
            Thread.sleep(100);
            System.gc();
            Thread.sleep(100);
            System.out.println("Warm-up complete.");
            System.out.println();

            // Test ClassCollector (old)
            System.out.println("Testing ClassCollector (old)...");
            long startOld = System.nanoTime();
            Map<String, String> resultOld = null;
            int successCountOld = 0;

            for (int i = 0; i < ITERATIONS; i++) {
                try {
                    ClassCollector collector = new ClassCollector();
                    resultOld = collector.getClasses(TEST_CLASS, true);
                    successCountOld++;
                    if ((i + 1) % 10 == 0) {
                        System.out.printf("  Progress: %3d/%d iterations (%.1f%%)%n", i + 1, ITERATIONS, (i + 1) * 100.0 / ITERATIONS);
                    }
                } catch (Exception e) {
                    System.err.println("Error at iteration " + (i + 1) + ": " + e.getMessage());
                }
            }
            long durationOld = System.nanoTime() - startOld;

            System.out.println("✓ ClassCollector completed");
            System.out.println();

            // Test ClassCollector2 (new)
            System.out.println("Testing ClassCollector2 (new)...");
            ClassCollector2.clearCache();
            long startNew = System.nanoTime();
            Map<String, String> resultNew = null;
            int successCountNew = 0;

            for (int i = 0; i < ITERATIONS; i++) {
                try {
                    ClassCollector2 collector = new ClassCollector2();
                    resultNew = collector.getClasses(TEST_CLASS, true);
                    successCountNew++;
                    if ((i + 1) % 10 == 0) {
                        System.out.printf("  Progress: %3d/%d iterations (%.1f%%)%n", i + 1, ITERATIONS, (i + 1) * 100.0 / ITERATIONS);
                    }
                } catch (Exception e) {
                    System.err.println("Error at iteration " + (i + 1) + ": " + e.getMessage());
                }
            }
            long durationNew = System.nanoTime() - startNew;

            System.out.println("✓ ClassCollector2 completed");
            System.out.println();

            // Results
            System.out.println("=================================================================");
            System.out.println("RESULTS");
            System.out.println("=================================================================");
            System.out.println();

            System.out.println("--- ClassCollector (old) ---");
            System.out.printf("  Successful scans:    %d / %d%n", successCountOld, ITERATIONS);
            if (resultOld != null) {
                System.out.printf("  Classes found:       %d%n", resultOld.size());
            }
            System.out.printf("  Total time:          %,12.2f ms%n", durationOld / 1_000_000.0);
            System.out.printf("  Average per scan:    %,12.2f ms%n", durationOld / ITERATIONS / 1_000_000.0);
            System.out.println();

            System.out.println("--- ClassCollector2 (new) ---");
            System.out.printf("  Successful scans:    %d / %d%n", successCountNew, ITERATIONS);
            if (resultNew != null) {
                System.out.printf("  Classes found:       %d%n", resultNew.size());
            }
            System.out.printf("  Total time:          %,12.2f ms%n", durationNew / 1_000_000.0);
            System.out.printf("  Average per scan:    %,12.2f ms%n", durationNew / ITERATIONS / 1_000_000.0);
            System.out.println();
            System.out.println("  " + ClassCollector2.getCacheStats());
            System.out.println();

            // Comparison
            System.out.println("--- COMPARISON ---");
            if (resultOld != null && resultNew != null) {
                if (resultOld.size() == resultNew.size()) {
                    System.out.println("  ✓ Both collectors found the same number of classes: " + resultOld.size());
                } else {
                    System.out.printf("  ⚠ Different results! Old: %d classes, New: %d classes%n",
                            resultOld.size(), resultNew.size());
                    
                    // Find differences
                    System.out.println();
                    System.out.println("  Analyzing differences...");
                    
                    // Classes only in NEW
                    java.util.Set<String> onlyInNew = new java.util.HashSet<String>(resultNew.keySet());
                    onlyInNew.removeAll(resultOld.keySet());
                    
                    // Classes only in OLD
                    java.util.Set<String> onlyInOld = new java.util.HashSet<String>(resultOld.keySet());
                    onlyInOld.removeAll(resultNew.keySet());
                    
                    if (!onlyInNew.isEmpty()) {
                        System.out.println();
                        System.out.println("  Classes found ONLY by ClassCollector2 (new): " + onlyInNew.size());
                        java.util.List<String> sortedNew = new java.util.ArrayList<String>(onlyInNew);
                        java.util.Collections.sort(sortedNew);
                        for (String className : sortedNew) {
                            System.out.println("    + " + className);
                        }
                    }
                    
                    if (!onlyInOld.isEmpty()) {
                        System.out.println();
                        System.out.println("  Classes found ONLY by ClassCollector (old): " + onlyInOld.size());
                        java.util.List<String> sortedOld = new java.util.ArrayList<String>(onlyInOld);
                        java.util.Collections.sort(sortedOld);
                        for (String className : sortedOld) {
                            System.out.println("    - " + className);
                        }
                    }
                    
                    // Check for hash differences in common classes
                    System.out.println();
                    System.out.println("  Checking hashes for common classes...");
                    int hashDifferences = 0;
                    for (String className : resultOld.keySet()) {
                        if (resultNew.containsKey(className)) {
                            String hashOld = resultOld.get(className);
                            String hashNew = resultNew.get(className);
                            if (!hashOld.equals(hashNew)) {
                                hashDifferences++;
                                if (hashDifferences <= 5) { // Only show first 5
                                    System.out.println("    ⚠ Hash mismatch for: " + className);
                                }
                            }
                        }
                    }
                    if (hashDifferences == 0) {
                        System.out.println("    ✓ All common classes have identical hashes");
                    } else if (hashDifferences > 5) {
                        System.out.println("    ... and " + (hashDifferences - 5) + " more hash differences");
                    }
                }
            }
            System.out.println();

            double speedup = (double) durationOld / durationNew;
            System.out.printf("  Speedup:             %.2fx %s%n",
                    speedup,
                    speedup > 1.0 ? "✓ NEW FASTER" : "✗ OLD FASTER");
            System.out.println();

            long timeSaved = durationOld - durationNew;
            System.out.printf("  Time saved:          %,12.2f ms (%.1f%% faster)%n",
                    timeSaved / 1_000_000.0,
                    (1.0 - 1.0 / speedup) * 100);
            System.out.println();

            // Cache efficiency
            if (ClassCollector2.getCacheSize() > 0) {
                System.out.println("--- CACHE EFFICIENCY ---");
                System.out.printf("  Unique classes:      %d%n", ClassCollector2.getCacheSize());
                if (resultNew != null && resultNew.size() > 0) {
                    System.out.printf("  Total scans:         %d classes × %d iterations = %d%n",
                            resultNew.size(), ITERATIONS, resultNew.size() * ITERATIONS);
                    double cacheHitRate = (1.0 - (double) ClassCollector2.getCacheSize() / (resultNew.size() * ITERATIONS)) * 100;
                    System.out.printf("  Cache hit rate:      %.2f%%%n", cacheHitRate);
                }
            }

            System.out.println();
            System.out.println("=================================================================");
            System.out.println("BENCHMARK COMPLETE");
            System.out.println("=================================================================");

        } catch (Exception e) {
            System.err.println("Benchmark failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
