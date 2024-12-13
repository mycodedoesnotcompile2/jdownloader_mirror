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
package org.appwork.processes.jna.tests;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.app.gui.BasicGui;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.jna.JNAWindowsProcessHandler;
import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Time;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.AbstractLineHandler;
import org.appwork.utils.processes.command.Command;

import com.sun.jna.platform.win32.WinDef.HWND;

/**
 * @author thomas
 * @date 19.11.2024
 *
 */
@TestDependency({ TestJNAWindowsProcessHandler.ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER, TestJNAWindowsProcessHandler.ORG_APPWORK_APP_GUI_BASIC_GUI })
public class TestJNAWindowsProcessHandler extends AWTest {
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_APP_GUI_BASIC_GUI                         = "org.appwork.app.gui.BasicGui";
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER = "org.appwork.processes.jna.JNAWindowsProcessHandler";

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // required for post build tests to auto-include requirements
        // this will throw a Class not found error, if BasicGui is not part of the build - and this will get ignored.
        // without this import, we will get an undefined error in the extra process spawned later - which cannot be evaluated by the Post
        // Script Runner correctly.
        BasicGui.class.getName();
        String unique = "awtest" + UniqueAlltimeID.create();
        JNAWindowsProcessHandler handler = new JNAWindowsProcessHandler();
        testSendCloseToWindow(unique, handler);
        testSendCTRL(unique, handler);
        testKill(unique, handler);
    }

    protected void testSendCTRL(String unique, ProcessHandler handler) throws IOException, InterruptedException, Exception {
        Command c = new Command("cmd", "/c", "ping localhost -n 1000 -" + unique);
        c.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                // System.out.println(line);
            }
        });
        c.start(true);
        try {
            ProcessInfo found = null;
            ArrayList<Integer> pids = new ArrayList<Integer>();
            Timeout timeout = new Timeout(5000);
            List<ProcessInfo> list = null;
            while (timeout.isAlive() && found == null) {
                pids.clear();
                list = handler.listByPath("C:\\Windows\\System32\\PING.EXE");
                for (ProcessInfo p : list) {
                    pids.add(p.getPid());
                    if (p.getCommandLine().contains("-" + unique)) {
                        found = p;
                    }
                }
            }
            assertTrue(found != null);
            List<ProcessInfo> list2 = handler.listByPids(Deser.get().convert(pids, TypeRef.INT_ARRAY));
            assertEqualsDeep(list, list2);
            handler.terminateRequest(found);
            if (handler.waitForExit(TimeSpan.parse("10s"), found).size() > 0) {
                throw new Exception("Could not kill");
            }
            assertFalse(handler.terminateRequest(found));
            list.remove(found);
            List<ProcessInfo> list3 = handler.listByPids(Deser.get().convert(pids, TypeRef.INT_ARRAY));
            assertEqualsDeep(list, list3);
        } finally {
            c.getProcess().destroyForcibly();
        }
    }

    protected void testKill(String unique, ProcessHandler handler) throws IOException, Exception {
        Timeout timeout = new Timeout(5000);
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                System.out.println(caller + ":" + line);
            }
        });
        command.start(true);
        try {
            List<ProcessInfo> list = handler.listByPath(CrossSystem.getJavaBinary());
            ProcessInfo instance = null;
            ArrayList<Integer> pids = new ArrayList<Integer>();
            while (timeout.isAlive() && instance == null) {
                pids.clear();
                list = handler.listByPath(CrossSystem.getJavaBinary());
                for (ProcessInfo p : list) {
                    pids.add(p.getPid());
                    if (p.getCommandLine().contains("-" + unique)) {
                        instance = p;
                    }
                }
            }
            assertTrue(instance != null);
            assertTrue(handler.terminateForced(instance, -1));
            if (handler.waitForExit(TimeSpan.parse("10s"), instance).size() > 0) {
                throw new Exception("Could not kill");
            }
            assertFalse(handler.terminateRequest(instance));
            List<ProcessInfo> list2 = handler.listByPath(CrossSystem.getJavaBinary());
            list.remove(instance);
            assertEqualsDeep(list2, list);
        } finally {
            command.destroy();
        }
    }

    protected void testSendCloseToWindow(String unique, JNAWindowsProcessHandler handler) throws IOException, Exception {
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        final AtomicReference<Boolean> received = new AtomicReference<Boolean>(false);
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                System.out.println(caller + ":" + line);
                if (line.contains("Received Request Exit")) {
                    received.set(true);
                }
            }
        });
        command.start(true);
        List<ProcessInfo> list = null;
        Timeout timeout = new Timeout(5000);
        ProcessInfo instance = null;
        while (timeout.isAlive() && instance == null) {
            list = handler.listByPath(CrossSystem.getJavaBinary());
            for (ProcessInfo p : list) {
                if (p.getCommandLine().contains("-" + unique)) {
                    instance = p;
                }
            }
        }
        assertTrue(instance != null);
        long started = Time.systemIndependentCurrentJVMTimeMillis();
        while (Time.systemIndependentCurrentJVMTimeMillis() - started < 10000) {
            HWND window = handler.findMainWindow(instance.getPid());
            if (window != null) {
                break;
            }
            Thread.sleep(1000);
        }
        handler.terminateRequest(instance);
        Thread.sleep(2000);
        assertTrue(received.get());
        List<ProcessInfo> remaining;
        if ((remaining = handler.waitForExit(TimeSpan.parse("10s"), instance)).size() > 0) {
            throw new Exception("Could not kill: " + remaining);
        }
        List<ProcessInfo> list2 = handler.listByPath(CrossSystem.getJavaBinary());
        list.remove(instance);
        assertFalse(handler.terminateRequest(instance));
        assertEqualsDeep(list2, list);
    }

    public static void main(String[] args) {
        run();
    }
}
