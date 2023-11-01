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
package org.appwork.utils.processes.test;

import java.io.IOException;
import java.util.ArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

/**
 * @author daniel
 * @date Jul 26, 2019
 *
 */
public class ProcessBuilderFactoryTest extends AWTest {

    public static void main(String[] args) throws IOException, InterruptedException {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        switch (CrossSystem.getOSFamily()) {
        case WINDOWS:
            runTestWindows();
            break;
        case LINUX:
            runTestLinux();
            break;
        default:
            break;
        }
    }

    private void runTestLinux() throws Exception {
        {
            LogV3.info("Test Execute ping -h");
            ArrayList<String> l = new ArrayList<String>();
            l.add("ping");
            l.add("-h");
            ProcessBuilder pb = ProcessBuilderFactory.create(l);
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
            assertEqualsDeep(result.getStdOutString(), "");
            assertEqualsDeep(result.getExitCode(), 2);
            assertTrue(result.getErrOutString().contains("Usage: ping"));
        }
    }

    private void runTestWindows() throws Exception {
        {
            LogV3.info("Test Execute ping /?");
            ArrayList<String> l = new ArrayList<String>();
            l.add("ping");
            l.add("/?");
            ProcessBuilder pb = ProcessBuilderFactory.create(l);
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
            assertEqualsDeep(result.getErrOutString(), "");
            assertEqualsDeep(result.getExitCode(), 0);
            assertTrue(result.getStdOutString().contains("Syntax: ping") || result.getStdOutString().contains("Usage: ping"));
        }
        {
            LogV3.info("Test Execute ping /BAD");
            ArrayList<String> l = new ArrayList<String>();
            l.add("ping");
            l.add("/BAD");

            ProcessBuilder pb = ProcessBuilderFactory.create(l);
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);

            assertEqualsDeep(result.getErrOutString(), "");
            assertEqualsDeep(result.getExitCode(), 1);

            assertTrue(result.getStdOutString().contains("Syntax: ping") || result.getStdOutString().contains("Usage: ping"));
        }
        {
            LogV3.info("Test Execute unknown command");
            ArrayList<String> l = new ArrayList<String>();
            l.add("dir213");
            try {
                ProcessBuilder pb = ProcessBuilderFactory.create(l);
                ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
                throw new Exception("Expected: java.io.IOException: Cannot run program ");
            } catch (IOException e) {
            }
        }
        {
            LogV3.info("Test Execute cmd /C with errout");
            ArrayList<String> l = new ArrayList<String>();
            l.add("cmd");
            l.add("/C");
            l.add("type");
            l.add("sadasdfasfad");
            ProcessBuilder pb = ProcessBuilderFactory.create(l);
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
            assertEqualsDeep(result.getStdOutString(), "");
            assertEqualsDeep(result.getExitCode(), 1);
            assertEqualsDeepNot(result.getErrOutString(), "");
        }
    }

}
