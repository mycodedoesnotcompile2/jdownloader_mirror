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
import java.nio.file.Files;
import java.nio.file.Path;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.ExtIOException;
import org.appwork.utils.Files17;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.WindowsUtils.AccessPermissionEntry;

/**
 * @author thomas
 * @date 30.01.2024
 *
 */
@TestDependency({ FilesTests.ORG_APPWORK_UTILS_OS_WINDOWS_UTILS })
public class FilesTests extends AWTest {
    @AWTestValidateClassReference
    static final String ORG_APPWORK_UTILS_OS_WINDOWS_UTILS = "org.appwork.utils.os.WindowsUtils";

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!JavaVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
            return;
        }
        File baseFile = Application.getTempUniqueResource("tests");
        Path basePath = baseFile.toPath();
        try {
            assertFalse(Files17.existsPath(basePath));
            assertFalse(Files17.existsDirectory(basePath));
            assertFalse(Files17.existsFile(basePath));
            baseFile.createNewFile();
            assertTrue(Files17.existsPath(basePath));
            assertFalse(Files17.existsDirectory(basePath));
            assertTrue(Files17.existsFile(basePath));
            Files.delete(basePath);
            assertFalse(Files17.existsPath(basePath));
            assertFalse(Files17.existsDirectory(basePath));
            assertFalse(Files17.existsFile(basePath));
            baseFile.mkdirs();
            final File subFolder = new File(baseFile, "subfolder");
            subFolder.mkdirs();
            final File subFile = new File(baseFile, "subfile");
            subFile.createNewFile();
            assertTrue(Files17.existsPath(basePath));
            assertTrue(Files17.existsDirectory(basePath));
            assertFalse(Files17.existsFile(basePath));
            if (CrossSystem.isWindows()) {
                File windowsConfig = new File(System.getenv("SystemRoot"), "System32\\config");
                final File fileInsideWindowsConfig = new File(windowsConfig, "SOFTWARE");
                final File dirInsideWindowsConfig = new File(windowsConfig, "RegBack");
                assertTrue(Files17.existsPath(windowsConfig.toPath()));
                assertTrue(Files17.existsDirectory(windowsConfig.toPath()));
                assertFalse(Files17.existsFile(windowsConfig.toPath()));
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsFile(dirInsideWindowsConfig.toPath());
                    }
                };
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsDirectory(dirInsideWindowsConfig.toPath());
                    }
                };
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsPath(dirInsideWindowsConfig.toPath());
                    }
                };
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsFile(fileInsideWindowsConfig.toPath());
                    }
                };
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsDirectory(fileInsideWindowsConfig.toPath());
                    }
                };
                new AssertAnException<ExtIOException>() {
                    @Override
                    protected void run() throws Exception {
                        Files17.existsPath(fileInsideWindowsConfig.toPath());
                    }
                };
                WindowsUtils.applyPermissions(baseFile, false, true, new AccessPermissionEntry(WindowsUtils.SID.SID_BUILTIN_ADMINISTRATORS.sid, true, WindowsUtils.PERMISSIONSET_FULL));
                try { // we still should be able to check the path
                    assertTrue(Files17.existsPath(basePath));
                    assertTrue(Files17.existsDirectory(basePath));
                    assertFalse(Files17.existsFile(basePath));
                    // but not the subfolders
                    new AssertAnException<ExtIOException>() {
                        @Override
                        protected void run() throws Exception {
                            Files17.existsPath(subFolder.toPath());
                        }
                    };
                    new AssertAnException<ExtIOException>() {
                        @Override
                        protected void run() throws Exception {
                            Files17.existsDirectory(subFolder.toPath());
                        }
                    };
                    new AssertAnException<ExtIOException>() {
                        @Override
                        protected void run() throws Exception {
                            Files17.existsFile(subFolder.toPath());
                        }
                    };
                } finally {
                    WindowsUtils.applyPermissions(baseFile, false, true, new AccessPermissionEntry(WindowsUtils.SID.SID_BUILTIN_USERS.sid, true, WindowsUtils.PERMISSIONSET_FULL));
                }
            }
        } finally {
            org.appwork.utils.Files.deleteRecursive(baseFile);
            assertFalse(Files17.existsPath(basePath));
            assertFalse(Files17.existsDirectory(basePath));
            assertFalse(Files17.existsFile(basePath));
        }
    }

    public static void main(String[] args) {
        run();
    }
}
