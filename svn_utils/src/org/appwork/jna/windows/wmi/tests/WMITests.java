/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.jna.windows.wmi.tests;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;

import org.appwork.jna.windows.wmi.JNAWMIUtils;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.os.CrossSystem;

/**
 * @author thomas
 * @date 20.11.2024
 *
 */
@TestDependency({ "org.appwork.jna.windows.wmi.JNAWMIUtils" })
public class WMITests extends AWTest {
    /**
     * @param args
     */
    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            System.out.println("Windows Only Test");
            return;
        }
        ArrayList<Map<String, Object>> a = JNAWMIUtils.query("ROOT\\CIMV2", "SELECT Caption from Win32_UserAccount");
        ArrayList<Map<String, Object>> b = JNAWMIUtils.wmiQueryViaPowerShell("ROOT\\CIMV2", "SELECT Caption from Win32_UserAccount");
        HashSet<String> contentsA = new HashSet<String>();
        HashSet<String> contentsB = new HashSet<String>();
        for (Map<String, Object> e : a) {
            contentsA.add((String) e.get("Caption"));
        }
        for (Map<String, Object> e : b) {
            contentsB.add((String) e.get("Caption"));
        }
        // may fail if a new process started
        assertEqualsDeep(contentsA, contentsB);
    }
}
