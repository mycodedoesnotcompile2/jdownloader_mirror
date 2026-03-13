/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.processes.jna.ide;

import java.lang.management.ManagementFactory;
import java.util.List;

import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.windows.jna.JNANonWMIWindowsProcessHandler;
import org.appwork.processes.windows.jna.JNAWindowsProcessHandler;
import org.appwork.utils.os.CrossSystem;

/**
 * IDE runnable that compares performance of {@link JNAWindowsProcessHandler} (WMI) vs
 * {@link JNANonWMIWindowsProcessHandler} (Toolhelp32). Prints timings for listByPath and listByPids
 * so you can see how the new non-WMI version performs.
 *
 * Run as Java Application on Windows. On non-Windows, exits without running.
 *
 * @author thomas
 * @date 08.03.2026
 */
public class ComparePerformance {

    private static final int WARMUP = 2;
    private static final int ITERATIONS = 10;

    public static void main(String[] args) {
        if (!CrossSystem.isWindows()) {
            System.out.println("Not Windows - skip. Run on Windows to compare WMI vs non-WMI process handlers.");
            return;
        }
        String javaPath = CrossSystem.getJavaBinary();
        int currentPid = getCurrentPid();
        if (javaPath == null || currentPid <= 0) {
            System.out.println("Could not get java path or current PID - skip.");
            return;
        }

        try {
            runComparison(javaPath, currentPid);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void runComparison(String javaPath, int currentPid) throws Exception {
        System.out.println("=== Compare JNAWindowsProcessHandler (WMI) vs JNANonWMIWindowsProcessHandler (Toolhelp32) ===\n");
        System.out.println("Java path: " + javaPath);
        System.out.println("Current PID: " + currentPid);
        System.out.println("Warmup: " + WARMUP + ", Iterations: " + ITERATIONS + "\n");

        JNAWindowsProcessHandler wmi = new JNAWindowsProcessHandler();
        JNANonWMIWindowsProcessHandler nonWmi = new JNANonWMIWindowsProcessHandler();

        // Warm-up
        for (int i = 0; i < WARMUP; i++) {
            wmi.listByPath(javaPath);
            wmi.listByPids(currentPid);
            nonWmi.listByPath(javaPath);
            nonWmi.listByPids(currentPid);
        }

        // listByPath(javaPath)
        long wmiPathTotal = 0;
        for (int i = 0; i < ITERATIONS; i++) {
            long t = System.currentTimeMillis();
            List<ProcessInfo> list = wmi.listByPath(javaPath);
            wmiPathTotal += System.currentTimeMillis() - t;
        }
        long nonWmiPathTotal = 0;
        for (int i = 0; i < ITERATIONS; i++) {
            long t = System.currentTimeMillis();
            List<ProcessInfo> list = nonWmi.listByPath(javaPath);
            nonWmiPathTotal += System.currentTimeMillis() - t;
        }

        // listByPids(currentPid)
        long wmiPidsTotal = 0;
        for (int i = 0; i < ITERATIONS; i++) {
            long t = System.currentTimeMillis();
            List<ProcessInfo> list = wmi.listByPids(currentPid);
            wmiPidsTotal += System.currentTimeMillis() - t;
        }
        long nonWmiPidsTotal = 0;
        for (int i = 0; i < ITERATIONS; i++) {
            long t = System.currentTimeMillis();
            List<ProcessInfo> list = nonWmi.listByPids(currentPid);
            nonWmiPidsTotal += System.currentTimeMillis() - t;
        }

        long wmiPathAvg = wmiPathTotal / ITERATIONS;
        long nonWmiPathAvg = nonWmiPathTotal / ITERATIONS;
        long wmiPidsAvg = wmiPidsTotal / ITERATIONS;
        long nonWmiPidsAvg = nonWmiPidsTotal / ITERATIONS;

        System.out.println("--- listByPath(\"java exe\") ---");
        System.out.println("  WMI    (JNAWindowsProcessHandler):     total " + wmiPathTotal + " ms, avg " + wmiPathAvg + " ms");
        System.out.println("  non-WMI (JNANonWMIWindowsProcessHandler): total " + nonWmiPathTotal + " ms, avg " + nonWmiPathAvg + " ms");
        if (wmiPathAvg > 0) {
            double ratio = (double) nonWmiPathAvg / (double) wmiPathAvg;
            System.out.println("  Ratio (non-WMI / WMI): " + String.format("%.2f", ratio) + " ( < 1 = non-WMI faster )");
        }
        System.out.println();

        System.out.println("--- listByPids(currentPid) ---");
        System.out.println("  WMI    (JNAWindowsProcessHandler):     total " + wmiPidsTotal + " ms, avg " + wmiPidsAvg + " ms");
        System.out.println("  non-WMI (JNANonWMIWindowsProcessHandler): total " + nonWmiPidsTotal + " ms, avg " + nonWmiPidsAvg + " ms");
        if (wmiPidsAvg > 0) {
            double ratio = (double) nonWmiPidsAvg / (double) wmiPidsAvg;
            System.out.println("  Ratio (non-WMI / WMI): " + String.format("%.2f", ratio) + " ( < 1 = non-WMI faster )");
        }
        System.out.println();

        System.out.println("=== Summary ===");
        if (nonWmiPathAvg < wmiPathAvg || nonWmiPidsAvg < wmiPidsAvg) {
            System.out.println("Non-WMI version is faster on at least one operation (typical on many Windows setups).");
        } else {
            System.out.println("WMI and non-WMI timings are in the same range. Run multiple times; first WMI call often pays one-time WMI init cost.");
        }
    }

    private static int getCurrentPid() {
        try {
            String name = ManagementFactory.getRuntimeMXBean().getName();
            int at = name.indexOf('@');
            if (at > 0) {
                return Integer.parseInt(name.substring(0, at));
            }
        } catch (Exception e) {
            // ignore
        }
        return -1;
    }
}
