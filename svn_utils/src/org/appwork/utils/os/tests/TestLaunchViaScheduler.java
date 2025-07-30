package org.appwork.utils.os.tests;

import java.util.HashSet;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Timeout;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;

public class TestLaunchViaScheduler extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (CrossSystem.isWindows()) {
            long id = UniqueAlltimeID.next();
            List<ProcessInfo> before = ProcessHandlerFactory.getProcessHandler().listByPath(null);
            HashSet<Integer> pids = new HashSet<Integer>();
            for (ProcessInfo p : before) {
                pids.add(p.getPid());
            }
            WindowsUtils.runViaWindowsScheduler("cmd.exe", null, "/c", "ping", "-w", id + "", "-n", "2000", "localhost");

            Timeout timeout = new Timeout(30000);
            boolean found = false;
            Exception exception = null;
            while (timeout.isAlive()) {
                Thread.sleep(500);
                List<ProcessInfo> processes = ProcessHandlerFactory.getProcessHandler().listByPath(null);
                for (ProcessInfo p : processes) {
                    if (pids.add(p.getPid())) {
                        System.out.println("new : " + p);
                        if (p.getCommandLine() != null && p.getCommandLine().contains(id + "")) {
                            LogV3.info("Found");
                            found = true;

                            if (WindowsUtils.isProcessElevated(p.getPid())) {
                                exception = new WTFException("Elevated! " + p);
                            }

                            System.out.println("Cancel " + p);
                            ProcessHandlerFactory.getProcessHandler().terminateForced(p, 1);

                        }

                    }
                }
                if (exception != null) {
                    throw exception;
                }
                if (found) {
                    return;
                }
            }

            throw new WTFException("Did not find process");
        }
    }

}
