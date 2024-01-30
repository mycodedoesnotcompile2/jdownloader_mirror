/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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

import static org.appwork.testframework.AWTest.logInfoAnyway;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.PostBuildTestInterface;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Files;
import org.appwork.utils.ZipContentHasher;
import org.appwork.utils.ZipContentHasher.Customizer;

/**
 * Test searches built jars for forbidden files like tests? or ide namespaces
 *
 */
public class AssertNoTestsInJars implements PostBuildTestInterface {
    private void validateZipOrJar(final File folder) throws Exception {
        for (final File f : Files.getFiles(true, true, folder)) {
            if (f.getName().toLowerCase(Locale.ROOT).endsWith(".jar") || f.getName().toLowerCase(Locale.ROOT).endsWith(".zip")) {
                if (f.getName().toLowerCase(Locale.ROOT).endsWith(".jar")) {
                    final String jarSigner = AWTest.verifyJar(f);
                    if (jarSigner.equals("jar is unsigned.")) {
                        logInfoAnyway("[WARNING] Unsigned JAR: " + f);
                    }
                }
                {
                    final FileInputStream is = new FileInputStream(f);
                    try {
                        AWTest.validateZipOrJar(f.getAbsolutePath(), is);
                    } finally {
                        is.close();
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(String[] args, File applicationRoot) throws Exception {
        validateZipOrJar(applicationRoot);
        for (final URL url : ClassPathScanner.getClassPath()) {
            // System.out.println(url);
            final File root = new File(url.toURI());
            if (root != null && root.isFile()) {
                ZipContentHasher.getContentSHA256(root, new Customizer() {
                    public boolean handle(String path, ZipInputStream zipStream, ZipEntry entry, HashMap<String, byte[]> results) throws IOException {
                        // add exceptions here
                        if (entry.getName().matches("this.is.an.allowed.test.path")) {
                            return false;
                        }
                        if (entry.getName().matches("org/bouncycastle/util/test/.*")) {
                            return false;
                        }
                        if (entry.getName().matches("net/sf/image4j/test/.*")) {
                            return false;
                        }
                        if (entry.getName().matches("(?i).*/tests?/.*")) {
                            throw new WTFException("Found test namespace: " + entry.getName() + " in " + root + " - " + path);
                        }
                        if (entry.getName().matches("(?i).*/ide/.*")) {
                            throw new WTFException("Found ide namespace: " + entry.getName() + " in " + root + " - " + path);
                        }
                        return false;
                    }
                });
            }
        }
    }
}
