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
package org.appwork.utils.os.tests;

import java.io.File;
import java.util.Iterator;
import java.util.Map;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;

@TestDependency({ "org.appwork.utils.os.WindowsUtils" })
public class TestWindowsCertificateCode extends AWTest {

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }

        String winDir = System.getenv("SystemRoot") + "\\";
        String sys32 = winDir + "System32\\";

        // ------------------------------------------------------------
        // Centralized definition of test targets
        // ------------------------------------------------------------
        TestTarget[] targets = new TestTarget[] { //

            new TestTarget(winDir + "explorer.exe", true, true, "Microsoft Windows Production PCA 2011"), //
            new TestTarget(sys32 + "taskmgr.exe", true, true, "Microsoft Windows Production PCA 2011"), //
            new TestTarget(winDir + "regedit.exe", false, false, "Microsoft Windows Production PCA 2011"), //
            new TestTarget(sys32 + "mmc.exe", false, false, "Microsoft Windows Production PCA 2011"), //
            new TestTarget(sys32 + "services.exe", true, true, "Microsoft Windows Production PCA 2011"), //

            new TestTarget(sys32 + "svchost.exe", true, true, "Microsoft Windows Production PCA 2011"), //
            new TestTarget(sys32 + "wuauclt.exe", true, true, "Microsoft Windows Production PCA 2011") //
        };

        // -------------------------------
        // Run all checks
        // -------------------------------
        for (int i = 0; i < targets.length; i++) {
            TestTarget t = targets[i];
            File f = new File(t.path);
            System.out.println("--------------------------------------------------");
            System.out.println("File: " + f.getAbsolutePath());

            Map info = WindowsUtils.readCodeSignSignature(f);
            boolean valid = WindowsUtils.verifySignature(f);

            // Print results
            if (info == null) {
                System.out.println("No certificate info found.");
            } else {
                for (Iterator it = info.entrySet().iterator(); it.hasNext();) {
                    Map.Entry e = (Map.Entry) it.next();
                    System.out.println("  " + e.getKey() + " = " + e.getValue());
                }
            }
            System.out.println("Signature valid: " + valid);

            // Validate against expected behavior
            validateResult(t, info, valid);
        }

        System.out.println("✅ All signature checks passed.");
    }

    /**
     * Checks if the actual result matches expected outcome. Throws an exception when an unexpected state is detected.
     */
    private void validateResult(TestTarget target, Map info, boolean valid) {
        String name = new File(target.path).getName();

        if (target.mustExist && (info == null || info.isEmpty())) {
            throw new IllegalStateException(name + ": certificate info expected but missing.");
        }

        if (target.mustBeValid && !valid) {
            throw new IllegalStateException(name + ": signature expected to be valid.");
        }

        if (info != null && target.expectedCN != null) {
            String cn = (String) info.get("CN");
            if (cn == null) {
                throw new IllegalStateException(name + ": CN missing in certificate info.");
            }
            if (cn.toLowerCase().indexOf(target.expectedCN.toLowerCase()) < 0) {
                throw new IllegalStateException(name + ": CN mismatch, expected part '" + target.expectedCN + "', got '" + cn + "'");
            }
        }
    }

    /**
     * Simple test definition for one file.
     */
    private static final class TestTarget {
        final String  path;
        final boolean mustExist;   // must have a certificate info
        final boolean mustBeValid; // must verify successfully
        final String  expectedCN;  // expected CN substring

        TestTarget(String path, boolean mustExist, boolean mustBeValid, String expectedCN) {
            this.path = path;
            this.mustExist = mustExist;
            this.mustBeValid = mustBeValid;
            this.expectedCN = expectedCN;
        }
    }

    public static void main(String[] args) {
        run();
    }
}
