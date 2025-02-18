/**
 *
 */
package org.appwork.jna.processes;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.utils.Application;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;

/**
 *
 */
public class CTRLSender {
    /**
     *
     */
    public static final int EXCEPTION         = 2;
    /**
     *
     */
    public static final int PROCESS_NOT_FOUND = 1;

    public static void main(final String[] args) {
        BuildDecisions.setEnabled(false);
        final int pid = Integer.parseInt(args[0]);
        try {
            Application.setApplication(".AppWorkUtils");
            final SimpleLoggerFactory fac = new SimpleLoggerFactory();
            fac.addSink(new LogToStdOutSink());
            LogV3.setFactory(fac);
            // free console, because we cannot attach to multiple consoles
            // https://blog.codetitans.pl/post/sending-ctrl-c-signal-to-another-application-on-windows/
            // this
            LogV3.info("Send CTRL " + pid + "(" + pid + ")");
            {
                final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_INFORMATION, false, pid);
                try {
                    if (processHandle == null) {
                        LogV3.info("Process handle gone. Fine");
                        System.exit(PROCESS_NOT_FOUND);
                        return;
                    } else {
                        // process handler still available check exitcode
                        final IntByReference exitCode = new IntByReference();
                        if (Kernel32.INSTANCE.GetExitCodeProcess(processHandle, exitCode)) {
                            if (exitCode.getValue() != WinNT.STILL_ACTIVE) {
                                LogV3.info("Process gone. Fine");
                                System.exit(PROCESS_NOT_FOUND);
                                return;
                            }
                        }
                    }
                } finally {
                    if (processHandle != null) {
                        Kernel32Ext.INSTANCE.CloseHandle(processHandle);
                    }
                }
            }
            try {
                Kernel32Ext.INSTANCE.FreeConsole();
                try {
                    Kernel32.INSTANCE.SetLastError(0);
                    if (Kernel32Ext.INSTANCE.AttachConsole(pid)) {
                        // disable ctrl handling - else GenerateConsoleCtrlEvent will kill ourself
                        Kernel32Ext.INSTANCE.SetConsoleCtrlHandler(null, true);
                        try {
                            Kernel32.INSTANCE.SetLastError(0);
                            final boolean success = Kernel32.INSTANCE.GenerateConsoleCtrlEvent(0, pid);
                            LogV3.info("CTRL Success Result: " + success);
                            if (!success) {
                                throw new Exception("Could not send CTRL");
                            }
                        } finally {
                            try {
                                // reenable ctrl handling
                                Kernel32Ext.INSTANCE.SetConsoleCtrlHandler(null, false);
                            } finally {
                                // free console
                                Kernel32.INSTANCE.FreeConsole();
                            }
                        }
                    } else {
                        final int error = Kernel32.INSTANCE.GetLastError();
                        if (error != 0) {
                            LogV3.info("Last Error: " + error);
                            throw new com.sun.jna.platform.win32.Win32Exception(error);
                        }
                    }
                } finally {
                }
            } finally {
            }
        } catch (final Throwable e) {
            LogV3.log(e);
            System.exit(EXCEPTION);
        }
    }
}
