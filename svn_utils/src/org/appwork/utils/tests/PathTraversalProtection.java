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
package org.appwork.utils.tests;

import java.io.File;
import java.io.IOException;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Files;
import org.appwork.utils.os.CrossSystem;

/**
 * @author thomas
 * @date 08.05.2024
 *
 */
public class PathTraversalProtection extends AWTest {
    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        File result = null;
        if (CrossSystem.isWindows()) {
            result = org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../c");
            assertEquals(result, new File("c:/a/b/c"));
            result = org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../../b/c");
            assertEquals(result, new File("c:/a/b/c"));
            result = org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "/allowed1/../allowed2");
            assertEquals(result, new File("c:/a/b/c/allowed2"));
            result = org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../../b/c/allowed.txt");
            assertEquals(result, new File("c:/a/b/c/allowed.txt"));
            try {
                org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../../b/c/allowed.txt../../../");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                // expected
            }
            try {
                org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../../b");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                // expected
            }
            try {
                org.appwork.utils.Files.createChildSecure(new File("c:/a/b/c"), "../forbidden");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                // expected
            }
        }
        if (CrossSystem.isUnix()) {
            // Assuming the base directory is a Linux-style path:
            File baseDir = new File("/path/to/source/folder");
            // Attempt to traverse outside the base directory
            try {
                Files.createChildSecure(baseDir, "../outside-folder/file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Using double-dot (parent directory) prefix
            try {
                Files.createChildSecure(baseDir, "../file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Attempt to use an absolute path directly
            try {
                result = Files.createChildSecure(baseDir, "/etc/passwd");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Complex path traversal using multiple directory steps
            try {
                Files.createChildSecure(baseDir, "subfolder/../../outside-folder/file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Symlink attempting to reach outside the base directory (must be pre-created)
            // cannot test this without creating an actual symlink
            // try {
            // Files.createChildSecure(baseDir, "symlink-to-outside-folder/file.txt");
            // throw new Exception("This should throw an exception");
            // } catch (IOException e) {
            // System.out.println("Caught expected IOException: " + e.getMessage());
            // }
        }
        if (CrossSystem.isWindows()) {
            File baseDir = new File("C:\\path\\to\\source\\folder");
            // Attempt to traverse outside the base directory
            try {
                Files.createChildSecure(baseDir, "..\\outside-folder\\file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Using double-dot (parent directory) prefix
            try {
                Files.createChildSecure(baseDir, "..\\file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Attempt to use an absolute path directly
            try {
                result = Files.createChildSecure(baseDir, "C:\\Windows\\System32\\config\\system");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Complex path traversal using multiple directory steps
            try {
                Files.createChildSecure(baseDir, "subfolder\\..\\..\\outside-folder\\file.txt");
                throw new Exception("This should throw an exception");
            } catch (IOException e) {
                System.out.println("Caught expected IOException: " + e.getMessage());
            }
            // Symlink attempting to reach outside the base directory (must be pre-created)
            // cannot test this without having an actual symlink
            // try {
            // Files.createChildSecure(baseDir, "symlink-to-outside-folder\\file.txt");
            // throw new Exception("This should throw an exception");
            // } catch (IOException e) {
            // System.out.println("Caught expected IOException: " + e.getMessage());
            // }
        }
    }

    public static void main(String[] args) {
        run();
    }
}
