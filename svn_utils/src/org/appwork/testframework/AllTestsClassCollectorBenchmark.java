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

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Files;

/**
 * Benchmark: Scans ALL test classes like IDETestRunner does and compares ClassCollector vs ClassCollector2
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class AllTestsClassCollectorBenchmark {

    public static void main(String[] args) {
        try {
            System.out.println("=================================================================");
            System.out.println("All Tests ClassCollector Benchmark");
            System.out.println("=================================================================");
            System.out.println();

            // Find all test classes
            System.out.println("Scanning classpath for test classes...");
            List<String> testClasses = findAllTestClasses();
            System.out.println("Found " + testClasses.size() + " test classes");
            System.out.println();

            if (testClasses.isEmpty()) {
                System.out.println("No test classes found. Exiting.");
                return;
            }

            // Limit to reasonable number for benchmark
            int maxTests = testClasses.size();
            if (testClasses.size() > maxTests) {
                Collections.shuffle(testClasses);
                testClasses = testClasses.subList(0, maxTests);
                System.out.println("Limited to " + maxTests + " test classes for benchmark");
                System.out.println();
            }

            System.out.println("Test classes to scan:");
            for (int i = 0; i < Math.min(10, testClasses.size()); i++) {
                System.out.println("  " + (i + 1) + ". " + testClasses.get(i));
            }
            if (testClasses.size() > 10) {
                System.out.println("  ... and " + (testClasses.size() - 10) + " more");
            }
            System.out.println();

            // Warm-up
            System.out.println("Warming up JVM...");
            for (int i = 0; i < 3; i++) {
                try {
                    new ClassCollector().getClasses("org.appwork.testframework.AWTest", true);
                    new ClassCollector2().getClasses("org.appwork.testframework.AWTest", true);
                } catch (Exception e) {
                    // Ignore
                }
            }
            Thread.sleep(100);
            System.gc();
            Thread.sleep(100);
            System.out.println("Warm-up complete.");
            System.out.println();

            // Benchmark ClassCollector (old)
            System.out.println("=================================================================");
            System.out.println("Testing ClassCollector (old)...");
            System.out.println("=================================================================");
            long startOld = System.nanoTime();
            Map<String, Map<String, String>> resultsOld = new HashMap<String, Map<String, String>>();
            int successCountOld = 0;
            int totalClassesOld = 0;

            for (int i = 0; i < testClasses.size(); i++) {
                String testClass = testClasses.get(i);
                try {
                    ClassCollector collector = new ClassCollector();
                    Map<String, String> result = collector.getClasses(testClass, true);
                    resultsOld.put(testClass, result);
                    totalClassesOld += result.size();
                    successCountOld++;

                    if ((i + 1) % 10 == 0 || i == 0) {
                        System.out.printf("  Progress: %3d/%d - %s (%d classes)%n", i + 1, testClasses.size(), testClass, result.size());
                    }
                } catch (Exception e) {
                    System.err.println("  Error scanning " + testClass + ": " + e.getMessage());
                }
            }
            long durationOld = System.nanoTime() - startOld;
            System.out.println("✓ ClassCollector completed");
            System.out.println();

            // Benchmark ClassCollector2 (new)
            System.out.println("=================================================================");
            System.out.println("Testing ClassCollector2 (new)...");
            System.out.println("=================================================================");
            ClassCollector2.clearCache();
            long startNew = System.nanoTime();
            Map<String, Map<String, String>> resultsNew = new HashMap<String, Map<String, String>>();
            int successCountNew = 0;
            int totalClassesNew = 0;

            for (int i = 0; i < testClasses.size(); i++) {
                String testClass = testClasses.get(i);
                try {
                    ClassCollector2 collector = new ClassCollector2();
                    Map<String, String> result = collector.getClasses(testClass, true);
                    resultsNew.put(testClass, result);
                    totalClassesNew += result.size();
                    successCountNew++;

                    if ((i + 1) % 10 == 0 || i == 0) {
                        System.out.printf("  Progress: %3d/%d - %s (%d classes)%n", i + 1, testClasses.size(), testClass, result.size());
                    }
                } catch (Exception e) {
                    System.err.println("  Error scanning " + testClass + ": " + e.getMessage());
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
            System.out.printf("  Successful scans:       %d / %d%n", successCountOld, testClasses.size());
            System.out.printf("  Total classes scanned:  %,d%n", totalClassesOld);
            System.out.printf("  Total time:             %,12.2f ms%n", durationOld / 1_000_000.0);
            System.out.printf("  Average per test:       %,12.2f ms%n", durationOld / testClasses.size() / 1_000_000.0);
            System.out.println();

            System.out.println("--- ClassCollector2 (new) ---");
            System.out.printf("  Successful scans:       %d / %d%n", successCountNew, testClasses.size());
            System.out.printf("  Total classes scanned:  %,d%n", totalClassesNew);
            System.out.printf("  Total time:             %,12.2f ms%n", durationNew / 1_000_000.0);
            System.out.printf("  Average per test:       %,12.2f ms%n", durationNew / testClasses.size() / 1_000_000.0);
            System.out.println();
            System.out.println("  " + ClassCollector2.getCacheStats());
            System.out.println();

            // Comparison
            System.out.println("--- PERFORMANCE COMPARISON ---");
            double speedup = (double) durationOld / durationNew;
            System.out.printf("  Speedup:                %.2fx %s%n", speedup, speedup > 1.0 ? "✓ NEW FASTER" : "✗ OLD FASTER");
            System.out.println();

            long timeSaved = durationOld - durationNew;
            System.out.printf("  Time saved:             %,12.2f ms (%.1f%% faster)%n", timeSaved / 1_000_000.0, (1.0 - 1.0 / speedup) * 100);
            System.out.println();

            // Class count comparison
            System.out.println("--- CLASS COUNT COMPARISON ---");
            System.out.printf("  Old total:              %,d classes%n", totalClassesOld);
            System.out.printf("  New total:              %,d classes%n", totalClassesNew);
            System.out.printf("  Difference:             %+,d classes%n", totalClassesNew - totalClassesOld);
            System.out.println();

            // Find global differences
            Set<String> allClassesOld = new HashSet<String>();
            for (Map<String, String> result : resultsOld.values()) {
                allClassesOld.addAll(result.keySet());
            }
            Set<String> allClassesNew = new HashSet<String>();
            for (Map<String, String> result : resultsNew.values()) {
                allClassesNew.addAll(result.keySet());
            }

            Set<String> onlyInNew = new HashSet<String>(allClassesNew);
            onlyInNew.removeAll(allClassesOld);
            Set<String> onlyInOld = new HashSet<String>(allClassesOld);
            onlyInOld.removeAll(allClassesNew);

            if (!onlyInNew.isEmpty() || !onlyInOld.isEmpty()) {
                System.out.println("--- UNIQUE CLASSES FOUND ---");
                if (!onlyInNew.isEmpty()) {
                    System.out.println("  Only in NEW: " + onlyInNew.size() + " classes");
                    List<String> sorted = new ArrayList<String>(onlyInNew);
                    Collections.sort(sorted);
                    for (int i = 0; i < Math.min(10, sorted.size()); i++) {
                        System.out.println("    + " + sorted.get(i));
                    }
                    if (sorted.size() > 10) {
                        System.out.println("    ... and " + (sorted.size() - 10) + " more");
                    }
                }
                if (!onlyInOld.isEmpty()) {
                    System.out.println("  Only in OLD: " + onlyInOld.size() + " classes");
                    List<String> sorted = new ArrayList<String>(onlyInOld);
                    Collections.sort(sorted);
                    for (int i = 0; i < Math.min(10, sorted.size()); i++) {
                        System.out.println("    - " + sorted.get(i));
                    }
                    if (sorted.size() > 10) {
                        System.out.println("    ... and " + (sorted.size() - 10) + " more");
                    }
                }
                System.out.println();
            }

            // Cache efficiency
            System.out.println("--- CACHE EFFICIENCY ---");
            int uniqueClasses = ClassCollector2.getCacheSize();
            System.out.printf("  Unique classes:         %,d%n", uniqueClasses);
            System.out.printf("  Total scans:            %,d%n", totalClassesNew);
            if (totalClassesNew > 0) {
                double deduplicationRate = (1.0 - (double) uniqueClasses / totalClassesNew) * 100;
                System.out.printf("  Deduplication:          %.2f%%%n", deduplicationRate);
                System.out.printf("  Average reuse:          %.2fx per class%n", (double) totalClassesNew / uniqueClasses);
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

    private static List<String> findAllTestClasses() {
        List<String> testClasses = new ArrayList<String>();
        try {
            for (URL url : ClassPathScanner.getClassPath()) {
                File root = new File(url.toURI());
                if (root.isDirectory()) {
                    System.out.println("  Scanning: " + root);
                    List<File> files = Files.getFiles(true, true, root);
                    for (File file : files) {
                        String rel = Files.getRelativePath(root, file);
                        if (rel.matches("(?i).*(test|ide).*\\.class$")) {
                            if (file.isFile()) {
                                String testClass = rel.replace("/", ".").replace("\\", ".").substring(0, rel.length() - ".class".length());
                                try {
                                    Class<?> cls = Class.forName(testClass, false, Thread.currentThread().getContextClassLoader());
                                    if (TestInterface.class.isAssignableFrom(cls) && TestInterface.class != cls && cls != AWTest.class) {
                                        testClasses.add(testClass);
                                    }
                                } catch (ClassNotFoundException e) {
                                    // Ignore - class not loadable
                                } catch (NoClassDefFoundError e) {
                                    // Ignore - dependency missing
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            System.err.println("Error finding test classes: " + e.getMessage());
        }
        return testClasses;
    }
}
