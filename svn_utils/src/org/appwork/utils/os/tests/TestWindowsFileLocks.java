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
import java.io.FileOutputStream;
import java.util.List;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.WindowsUtils.LockInfo;

/**
 * Test suite for Windows file lock detection functionality. Tests the ability to identify which processes have file handles/locks on
 * files.
 *
 * @author thomas
 * @date 19.01.2025
 */
@TestDependency({ "org.appwork.utils.os.WindowsUtils" })
public class TestWindowsFileLocks extends AWTest {
    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }
        testHandleBlocks();
    }

    /**
     * Test file handle/lock detection functionality on Windows.
     * 
     * This test verifies that {@link WindowsUtils#getLocksOnPath(File)} correctly identifies processes that have file handles/locks on
     * files. It performs two verification steps:
     * 
     * <h3>1. Self-Lock Verification (java.exe) - INFORMATIONAL ONLY</h3>
     * First, it attempts to verify that the currently running Java process has a lock on its own executable (java.exe) by calling
     * {@link WindowsUtils#getLocksOnPath(File)}. However, this check is NOT mandatory and only serves as additional verification,
     * because:
     * <ul>
     * <li><b>Memory-mapped execution</b>: Windows can execute programs memory-mapped without holding an exclusive file lock</li>
     * <li><b>JVM launcher architecture</b>: Many Java distributions use java.exe as a launcher that loads the actual JVM from DLLs. The
     * launcher executable itself may not remain locked after startup</li>
     * <li><b>Different JVM implementations</b>: Oracle JDK, OpenJDK, GraalVM, etc. behave differently regarding executable locks</li>
     * </ul>
     * Therefore, this part only logs informational messages but does not fail the test. It is common and expected that no lock is found
     * on modern JVM implementations.
     * 
     * <h3>2. Temporary File Lock Test - CRITICAL</h3>
     * This is the actual critical test that verifies lock detection works reliably for regular file operations:
     * <ul>
     * <li>Creates a temporary test file and writes initial content</li>
     * <li>Opens a {@link FileOutputStream} in append mode, which creates an exclusive lock</li>
     * <li>Runs {@link WindowsUtils#getLocksOnPath(File)} 10 times in a loop to verify consistency</li>
     * <li>Each iteration verifies:
     * <ul>
     * <li>Exactly one lock exists on the file</li>
     * <li>The lock belongs to the current process (PID matches)</li>
     * </ul>
     * </li>
     * <li>Properly closes the stream and cleans up the temporary file</li>
     * </ul>
     * 
     * <h3>Purpose</h3>
     * This test ensures that the Windows-specific file locking detection mechanism works correctly, which is critical for:
     * <ul>
     * <li>Identifying which processes are blocking file operations</li>
     * <li>Debugging file access issues</li>
     * <li>Implementing safe file replacement/update strategies</li>
     * </ul>
     * 
     * @throws Exception
     *             if any assertion fails or file operations encounter errors
     */
    private void testHandleBlocks() throws Exception {
        // Get the PID of the current Java process
        long myPID = CrossSystem.getPID();
        // Try to check if the running java.exe has a lock on itself
        // NOTE: This is INFORMATIONAL ONLY and may not work on all systems/JVM distributions
        File javaExe = new File(CrossSystem.getJavaBinary());
        List<LockInfo> myself = WindowsUtils.getLocksOnPath(javaExe);
        boolean foundViaLockDetection = false;
        for (LockInfo l : myself) {
            if (l.getPid() == myPID) {
                foundViaLockDetection = true;
                break;
            }
        }
        // Summary
        if (foundViaLockDetection) {
            logInfoAnyway("java.exe lock detection: SUCCESS - Lock found for current process (PID " + myPID + ")");
        } else {
            logInfoAnyway("java.exe lock detection: No lock found for current process (PID " + myPID + ")");
            logInfoAnyway("Note: This is normal for many JVM implementations due to:");
            logInfoAnyway("  - Memory-mapped execution (no exclusive file lock needed)");
            logInfoAnyway("  - Launcher-based architecture (java.exe only launches JVM from DLLs)");
            logInfoAnyway("  - Different JVM implementations (Oracle, OpenJDK, GraalVM handle this differently)");
            if (myself.size() > 0) {
                logInfoAnyway("  Found " + myself.size() + " lock(s) from other processes on: " + javaExe);
            }
        }
        // Create a temporary file to test lock detection
        File file = Application.getTempFile("tests", Time.now() + "");
        try {
            IO.secureWrite(file, "abc".getBytes());
            // Open a FileOutputStream which will create a lock on the file
            FileOutputStream fos = new FileOutputStream(file, true);
            try {
                System.out.println("Run findLockingPids");
                // Run the lock detection 10 times to verify consistency
                for (int i = 0; i < 10; i++) {
                    List<LockInfo> lockedBy = WindowsUtils.getLocksOnPath(file);
                    // Should find exactly one lock (our FileOutputStream)
                    assertTrue(lockedBy.size() == 1);
                    // The lock should belong to our process
                    assertThat(myPID).isNumber(lockedBy.get(0).getPid());
                    logInfoAnyway("Round " + i);
                }
            } finally {
                fos.close();
            }
        } finally {
            file.delete();
        }
    }

    public static void main(String[] args) {
        run();
    }
}
