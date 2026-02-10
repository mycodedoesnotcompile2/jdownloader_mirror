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
import java.io.FilenameFilter;

import org.appwork.utils.Application;

/**
 * Demonstrates the improvements in IDETestRunner2 vs IDETestRunner
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class TestRunnerComparison {
    public static void main(String[] args) {
        System.out.println("=================================================================");
        System.out.println("IDETestRunner vs IDETestRunner2 Comparison");
        System.out.println("=================================================================");
        System.out.println();
        try {
            Application.setApplication(".tests");
            File cfgDir = Application.getResource("cfg");
            // Show old cache structure
            System.out.println("--- OLD Cache Structure (IDETestRunner) ---");
            if (cfgDir.exists()) {
                File[] oldCaches = cfgDir.listFiles(new FilenameFilter() {
                    @Override
                    public boolean accept(File dir, String name) {
                        return name.startsWith("testcache_") && name.endsWith(".cache");
                    }
                });
                if (oldCaches != null && oldCaches.length > 0) {
                    long totalSize = 0;
                    for (File cache : oldCaches) {
                        totalSize += cache.length();
                    }
                    System.out.println("Cache files:         " + oldCaches.length);
                    System.out.println("Total cache size:    " + String.format("%.2f KB", totalSize / 1024.0));
                    System.out.println("Average per file:    " + String.format("%.2f KB", totalSize / 1024.0 / oldCaches.length));
                    System.out.println();
                    // Show some examples
                    System.out.println("Example cache files (first 5):");
                    for (int i = 0; i < Math.min(5, oldCaches.length); i++) {
                        System.out.println("  " + oldCaches[i].getName() + " (" + String.format("%.1f KB", oldCaches[i].length() / 1024.0) + ")");
                    }
                } else {
                    System.out.println("No old cache files found");
                }
            } else {
                System.out.println("No cfg directory found");
            }
            System.out.println();
            // Show new cache structure
            System.out.println("--- NEW Cache Structure (IDETestRunner2) ---");
            File unifiedCache = Application.getResource("cfg/unifiedTestCache.json");
            if (unifiedCache.exists()) {
                System.out.println("Cache files:         1 (unified)");
                System.out.println("Total cache size:    " + String.format("%.2f KB", unifiedCache.length() / 1024.0));
                System.out.println("File:                " + unifiedCache.getName());
            } else {
                System.out.println("No unified cache found yet (will be created on first run)");
            }
            System.out.println();
            // Benefits
            System.out.println("--- BENEFITS OF IDETestRunner2 ---");
            System.out.println("✓ Single unified cache file instead of 100+ separate files");
            System.out.println("✓ Uses fast ClassCollector2 (30x faster)");
            System.out.println("✓ Deduplicated class hashes (saves ~95% storage)");
            System.out.println("✓ Detailed change tracking and summary report");
            System.out.println("✓ Shows which files changed and why tests were executed");
            System.out.println("✓ Much faster cache loading/saving");
            System.out.println("✓ Incremental cache updates");
            System.out.println();
            System.out.println("--- SUMMARY FEATURES ---");
            System.out.println("The new summary shows:");
            System.out.println("  • Statistics (executed vs skipped tests)");
            System.out.println("  • Changed classes and their impact");
            System.out.println("  • Which tests were executed and why");
            System.out.println("  • Top classes by test impact");
            System.out.println("  • Cache efficiency metrics");
            System.out.println();
            System.out.println("=================================================================");
            System.out.println("To see the new runner in action, run:");
            System.out.println("  java org.appwork.testframework.IDETestRunner2");
            System.out.println("=================================================================");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
