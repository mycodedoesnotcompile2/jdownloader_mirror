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

import java.util.List;
import java.util.Set;

import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.swing.dialog.Dialog;

import com.sun.jna.platform.win32.Advapi32Util.Account;
import com.sun.jna.platform.win32.WinDef.INT_PTR;

/**
 * @author thomas
 * @date 20.11.2024
 *
 */
public class TestWindowsUtils extends AWTest {
    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }
        Set<Account> accounts = WindowsUtils.getCurrentUsersAccounts();
        Account me = WindowsUtils.getCurrentUserAccount();
        boolean sidFound = false;
        for (Account a : accounts) {
            if (me.sidString.equals(a.sidString)) {
                sidFound = true;
            }
        }
        assertTrue(sidFound);
        // at least me and a group
        assertThat(accounts.size()).isHigherThan(2);
        // we assme an admin account runs this test
        assertThat(WindowsUtils.isAdministrator());
        // we assume that we do not run the test elevated
        assertThat(!WindowsUtils.isElevated());
        assertThat(WindowsUtils.getMyPrincipalNames().size()).is(accounts.size());
        assertTrue(WindowsUtils.isCurrentUserPartOfGroup(WindowsUtils.SID.SID_AUTHENTICATED_USERS.sid));
        Dialog.I().showMessageDialog("The TestWindowUtils Test will now try to start cmd.exe via UAC");
        // Test with a known Windows executable
        long started = Time.now();
        INT_PTR processHandle = WindowsUtils.startElevatedProcess(new String[] { "cmd.exe", "-n", "ping", "heise.de" }, null, false);
        assertTrue(Time.now() - started > 1000);
        // Get and display the process ID
        int pid = WindowsUtils.getProcessId(processHandle);
        assertThat(pid).isHigherThan(0);
        List<ProcessInfo> processes = ProcessHandlerFactory.getProcessHandler().listByPids(pid);
        System.out.println("Started process with PID: " + pid);
        assertThat(processes.size()).is(1);
        // Check if process is elevated
        boolean isElevated = WindowsUtils.isProcessElevated(pid);
        assertTrue(isElevated);
        System.out.println("Process is running with elevated privileges: " + isElevated);
        // Wait a bit to see the process
        Thread.sleep(1000);
        boolean killed = WindowsUtils.terminateProcess(processHandle, 1);
        assertTrue(killed);
        List<ProcessInfo> processes2 = ProcessHandlerFactory.getProcessHandler().listByPids(pid);
        assertTrue(processes2.size() == 0);
    }

    public static void main(String[] args) {
        run();
    }
}
