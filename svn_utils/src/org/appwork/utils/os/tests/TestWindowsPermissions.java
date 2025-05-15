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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Joiner;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.WindowsUtils.AccessPermission;
import org.appwork.utils.os.WindowsUtils.AccessPermissionEntry;

import com.sun.jna.platform.win32.Advapi32Util.Account;

/**
 * Test suite for Windows permissions functionality. Tests all functionality related to Windows file permissions.
 *
 * @author thomas
 * @date 20.11.2024
 */
public class TestWindowsPermissions extends AWTest {
    private static final String TEST_DIR = System.getProperty("java.io.tmpdir") + "/WindowsPermissionsTest2";
    private File                testDir;

    /**
     * Test basic file access permissions
     */
    public void testBasicFileAccess() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Create test directory
        testDir = new File(TEST_DIR);
        if (!testDir.exists()) {
            testDir.mkdirs();
        }
        try {
            // Test basic read/write permissions
            AccessPermission[] basicPermissions = { AccessPermission.FILE_READ_DATA, AccessPermission.FILE_WRITE_DATA };
            // Check if we have basic access
            boolean hasAccess = WindowsUtils.checkFileAccessViaHandle(testDir, basicPermissions);
            assertTrue(hasAccess);
            // Verify available permissions
            Set<AccessPermission> availablePermissions = WindowsUtils.getAvailableAccessPermissions(testDir);
            assertNotNull(availablePermissions);
            assertTrue(availablePermissions.contains(AccessPermission.FILE_READ_DATA));
            assertTrue(availablePermissions.contains(AccessPermission.FILE_WRITE_DATA));
        } finally {
            // Clean up test directory
            if (testDir != null && testDir.exists()) {
                testDir.delete();
            }
        }
    }

    /**
     * Test setting and getting file permissions
     */
    public void testSetAndGetPermissions() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Create test directory
        testDir = new File(TEST_DIR);
        if (testDir.exists()) {
            if (!testDir.delete()) {
                throw new WTFException("Could not delete folder");
            }
        }
        testDir.mkdirs();
        try {
            Account owner = WindowsUtils.getFileOwnerSid(testDir);
            // Get current user's SID
            // WindowsUtils.isProcessElevated((int) CrossSystem.getPID());
            // WindowsUtils.getAvailableAccessPermissions(testDir);
            // this should work - but it will - of course - not change anything
            WindowsUtils.setFileOwner(testDir, owner.sidString);
            Account newOwner = WindowsUtils.getFileOwnerSid(testDir);
            assertEquals(newOwner.sidString, owner.sidString);
            AccessPermissionEntry[] is = WindowsUtils.getFileAccess(testDir);
            String currentUserSid = WindowsUtils.getCurrentUserSID();
            assertNotNull(currentUserSid);
            // AccessPermission.WRITE_DAC,AccessPermission.READ_CONTROL,AccessPermission.FILE_READ_ATTRIBUTES. the owner has this permission
            // implicit anyway. add it to avoid errors later
            Set<AccessPermission> testPermissions = new HashSet<AccessPermission>(Arrays.asList(AccessPermission.WRITE_DAC, AccessPermission.READ_CONTROL, AccessPermission.FILE_READ_ATTRIBUTES, AccessPermission.SYNCHRONIZE, AccessPermission.FILE_LIST_DIRECTORY, AccessPermission.FILE_READ_DATA, AccessPermission.FILE_ADD_FILE, AccessPermission.FILE_WRITE_DATA, AccessPermission.FILE_DELETE_CHILD, AccessPermission.DELETE));
            // Create test permissions
            // Create permission entry
            AccessPermissionEntry entry = new AccessPermissionEntry(currentUserSid, testPermissions, true, true);
            WindowsUtils.applyPermissions(testDir.getAbsolutePath(), true, false, Arrays.asList(entry));
            AccessPermissionEntry[] after = WindowsUtils.getFileAccess(testDir);
            assertTrue(after.length == is.length + 1);
            // Set permissions
            WindowsUtils.applyPermissions(testDir.getAbsolutePath(), false, true, Arrays.asList(entry));
            after = WindowsUtils.getFileAccess(testDir);
            assertTrue(after.length == 1);
            HashSet<String> failed = new HashSet<String>();
            for (AccessPermission p : AccessPermission.values()) {
                if (testPermissions.contains(p)) {
                    System.out.println("Check if " + p + " is set (via Handle)");
                    boolean checkAccess = WindowsUtils.checkFileAccessViaACL(testDir, p);
                    boolean handle = WindowsUtils.checkFileAccessViaHandle(testDir, p);
                    if (!checkAccess) {
                        failed.add("SetCheck Failed via CheckAccess " + p);
                    }
                    if (!handle) {
                        failed.add("SetCheck Failed via Handle " + p);
                    }
                } else {
                    System.out.println("Check if " + p + " is unset (via Handle)");
                    boolean checkAccess = WindowsUtils.checkFileAccessViaACL(testDir, p);
                    boolean handle = WindowsUtils.checkFileAccessViaHandle(testDir, p);
                    if (checkAccess) {
                        failed.add("UNSetCheck Failed via CheckAccess " + p);
                    }
                    if (handle) {
                        failed.add("UNSetCheck Failed via Handle " + p);
                    }
                }
            }
            if (failed.size() > 0) {
                logInfoAnyway(new Joiner("\r\n").join(failed));
                throw new WTFException(failed + "");
            }
            // Get and verify permissions
            // Find our entry
            boolean foundEntry = false;
            for (AccessPermissionEntry retrievedEntry : after) {
                if (retrievedEntry.getSid().equals(currentUserSid)) {
                    foundEntry = true;
                    assertEquals(testPermissions, retrievedEntry.getPermissions());
                    assertTrue(retrievedEntry.isAllow());
                    assertTrue(retrievedEntry.isInherit());
                    assertFalse(retrievedEntry.isInherited());
                    break;
                }
            }
            assertTrue(foundEntry);
        } finally {
            // Clean up test directory
            if (!testDir.delete()) {
                throw new WTFException("Could not Delete");
            }
        }
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }
        testBasicFileAccess();
        testSetAndGetPermissions();
    }

    public static void main(String[] args) {
        run();
    }
}