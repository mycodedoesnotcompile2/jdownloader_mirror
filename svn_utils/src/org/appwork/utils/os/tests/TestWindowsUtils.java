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
import java.util.List;
import java.util.Set;

import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;

import com.sun.jna.platform.win32.Advapi32Util.Account;
import com.sun.jna.platform.win32.WinDef.INT_PTR;

/**
 * Test suite for WindowsUtils class. Tests all functionality of the WindowsUtils utility class.
 *
 * @author thomas
 * @date 20.11.2024
 */
@TestDependency({ "org.appwork.utils.os.WindowsUtils" })
public class TestWindowsUtils extends AWTest {
    private static final String TEST_DIR = System.getProperty("java.io.tmpdir") + "/WindowsUtilsTest";
    private File                testDir;

    /**
     * Test user account and SID related functionality
     */
    public void testUserAccountAndSID() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Create test directory
        testDir = new File(TEST_DIR);
        if (!testDir.exists()) {
            testDir.mkdirs();
        }
        try {
            // Test getCurrentUserAccount
            Account currentUser = WindowsUtils.getCurrentUserAccount();
            assertNotNull(currentUser);
            assertNotNull(currentUser.sidString);
            // Test getCurrentUsersAccounts
            Set<Account> accounts = WindowsUtils.getCurrentUsersAccounts();
            assertNotNull(accounts);
            assertTrue(accounts.size() > 0);
            // Verify current user is in accounts
            boolean sidFound = false;
            for (Account a : accounts) {
                if (currentUser.sidString.equals(a.sidString)) {
                    sidFound = true;
                    break;
                }
            }
            assertTrue(sidFound);
            // Test getCurrentUserSID
            String sid = WindowsUtils.getCurrentUserSID();
            assertNotNull(sid);
            assertEquals(sid, currentUser.sidString);
            // Test getMyPrincipalNames
            Set<String> principalNames = WindowsUtils.getMyPrincipalNames();
            assertNotNull(principalNames);
            assertTrue(principalNames.size() > 0);
            assertEquals(principalNames.size(), accounts.size());
        } finally {
            // Clean up test directory
            if (testDir != null && testDir.exists()) {
                testDir.delete();
            }
        }
    }

    /**
     * Test administrator and elevation related functionality
     */
    public void testAdministratorAndElevation() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Test isAdministrator
        boolean isAdmin = WindowsUtils.isAdministrator();
        // Note: This test might fail depending on how the test is run
        logInfoAnyway("Is Administrator: " + isAdmin);
        // Test isElevated
        boolean isElevated = WindowsUtils.isElevated();
        // Note: This test might fail depending on how the test is run
        logInfoAnyway("Is Elevated: " + isElevated);
        // Test isCurrentUserPartOfGroup
        assertTrue(WindowsUtils.isCurrentUserPartOfGroup(WindowsUtils.SID.SID_AUTHENTICATED_USERS.sid));
    }

    /**
     * Test process management functionality
     */
    public void testProcessManagement() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Dialog.I().showMessageDialog("The TestWindowUtils Test will now try to start cmd.exe via UAC");
        logInfoAnyway("The TestWindowUtils Test will now try to start cmd.exe via UAC");
        // Test startElevatedProcess
        long started = Time.now();
        INT_PTR processHandle = WindowsUtils.startElevatedProcess(new String[] { "cmd.exe", "/c", "ping", "-n", "100", "heise.de" }, null, false);
        assertNotNull(processHandle);
        assertTrue(Time.now() - started > 1000);
        // Test getProcessId
        int pid = WindowsUtils.getProcessId(processHandle);
        assertTrue(pid > 0);
        // Verify process exists
        List<ProcessInfo> processes = ProcessHandlerFactory.getProcessHandler().listByPids(pid);
        assertEquals(processes.size(), 1);
        // Test isProcessElevated
        boolean isElevated = WindowsUtils.isProcessElevated(pid);
        assertTrue(isElevated);
        // Wait a bit to see the process
        Thread.sleep(1000);
        // Test terminateProcess
        boolean killed = WindowsUtils.terminateProcess(processHandle, 1);
        assertTrue(killed);
        // Verify process is gone
        List<ProcessInfo> processesAfter = ProcessHandlerFactory.getProcessHandler().listByPids(pid);
        assertTrue(processesAfter.isEmpty());
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
        testUserAccountAndSID();
        testAdministratorAndElevation();
        testProcessManagement();
    }

    public static void main(String[] args) {
        run();
    }
}
