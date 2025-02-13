/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.testframework.tests;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.Files.Handler;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ide.IDEUtils;

/**
 * @author thomas
 * @date 18.10.2024
 *
 */
public class ScanHeaderTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            return;
        }
        final ArrayList<File> modified = new ArrayList<File>();
        Files.walkThroughStructure(new Handler<Exception>() {
            @Override
            public void intro(File f) throws Exception {
            }

            @Override
            public void onFile(File f) throws Exception {
                if (f.isFile() && f.getName().endsWith(".java")) {
                    if (f.getName().equals((ScanHeaderTest.class.getSimpleName() + ".java"))) {
                        return;
                    }
                    if (f.lastModified() > System.currentTimeMillis() - 7 * 24 * 60 * 60 * 1000l) {
                        String java = IO.readFileToString(f);
                        int year = new Date().getYear() + 1900;
                        String newJava = java.replaceAll("Copyright \\(c\\) 2009-20\\d\\d,", "Copyright (c) 2009-" + year + ",");
                        newJava = newJava.replace("Schwabacher Straße 117", "Spalter Strasse 58");
                        newJava = newJava.replace("90763 Fürth", "91183 Abenberg");
                        if (!newJava.contains("e-mail@appwork.org")) {
                            newJava = newJava.replace("91183 Abenberg", "91183 Abenberg\r\n *         e-mail@appwork.org");
                        }
                        if (!newJava.equals(java)) {
                            IO.secureWrite(f, newJava, SYNC.META_AND_DATA);
                            modified.add(f);
                        }
                    }
                }
            }

            @Override
            public void outro(File f) throws Exception {
            }
        }, new File(IDEUtils.getWorkSpace(), "AppWorkUtils"));
        if (modified.size() > 0) {
            LogV3.info("This tests finds and corrects AppWorkUtils headers in modified files. You do not have to change anything - simply re-run the test");
            throw new Exception("Found and fixed incorrect (Abenberg)header: \r\n" + StringUtils.join(modified, "\r\n"));
        }
    }
}
