/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.List;
import java.util.Set;
import java.nio.charset.Charset;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.IO;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.ContinuesFileLineReader;
import org.appwork.utils.processes.LineHandler;
import org.appwork.utils.singleapp.Response;
import org.appwork.utils.singleapp.ResponseSender;

import com.sun.jna.Function;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.OleAuto;
import com.sun.jna.platform.win32.Variant;
import com.sun.jna.platform.win32.Variant.VARIANT;
import com.sun.jna.platform.win32.WTypes.BSTR;
import com.sun.jna.platform.win32.Guid.GUID;
import com.sun.jna.platform.win32.WinNT.HRESULT;

/**
 * Runs a command or task as NT AUTHORITY\SYSTEM (LocalSystem) via the Windows Task Scheduler 2.0 COM API (JNA) instead of schtasks.exe. Use
 * when JNA is available and {@link AdminHelperProcess#USE_JNA_FOR_LOCAL_SYSTEM_TASKS} is true. Same semantics as {@link AdminHelperProcess}
 * schtasks path: create task from XML, run, poll stdout/stderr/exitcode, respond with CMD_RESULT or TASK_RESULT, delete task.
 * <p>
 * Requires JNA and jna-platform (Ole32, OleAuto, Variant, Guid). If COM or JNA fails, the caller should fall back to schtasks.
 */
public final class RunAsLocalSystemJNA {
    private static final Charset UTF8                  = Charset.forName("UTF-8");
    /** TASK_CREATE_OR_UPDATE = 6 */
    private static final int                      TASK_CREATE_OR_UPDATE = 6;
    /** TASK_LOGON_NONE = 0 (use principal from XML) */
    private static final int                      TASK_LOGON_NONE       = 0;
    /** Delete task flag 0 */
    private static final int                      TASK_DELETE_FLAG      = 0;

    /**
     * Runs the same flow as runAsLocalSystemViaSchtasks but using the Task Scheduler COM API. Caller must add taskName to
     * activeLocalSystemTasks before calling and remove in finally; same for PIDs in activeLocalSystemPids.
     * Always waits and sends CMD_RESULT or TASK_RESULT with real output; for CMD (resultHexDir null) when waitFor is false,
     * sends CMD_RESULT immediately after PROCESS_STARTED and returns.
     */
    public static void runAsLocalSystemViaJna(ResponseSender callback, File workDir, String[] cmd, boolean waitFor, boolean keepRunning, String requestId, File resultHexDir, File taskBinFile, boolean verboseLog, Set<String> activeLocalSystemTasks, List<Integer> activeLocalSystemPids) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("RunAsLocalSystemJNA is Windows-only");
        }
        if (!JNAHelper.isJNAAvailable()) {
            throw new UnsupportedOperationException("JNA is not available");
        }
        File tempDir = (workDir != null && workDir.isDirectory()) ? workDir : getRunAsLocalSystemTempDir();
        if (workDir == null || !workDir.isDirectory()) {
            tempDir.mkdirs();
        }
        AdminHelperProcess.restrictTempDirToAdminAndSystem(tempDir);
        String workDirPath = (workDir != null && workDir.isDirectory()) ? workDir.getAbsolutePath() : tempDir.getAbsolutePath();
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        if (!waitFor) {
            LogV3.info("Execute as Local System (JNA): " + commandLine);
            commandLine = "start /b \"\" " + commandLine;
        } else {
            LogV3.info("Execute and wait as Local System (JNA): " + commandLine);
        }
        String tempPath = tempDir.getAbsolutePath().replace("\"", "\"\"");
        String wrapperArgs = "/c \"chcp 65001 >nul && cd /d \"" + workDirPath.replace("\"", "\"\"") + "\" && " + commandLine + " > \"" + tempPath + "\\stdout.txt\" 2> \"" + tempPath + "\\stderr.txt\" & echo %ERRORLEVEL% > \"" + tempPath + "\\exitcode.txt\"\"";
        String xml = WindowsUtils.buildTaskXmlAsLocalSystem("cmd.exe", wrapperArgs, tempDir.getAbsolutePath());
        File xmlFile = new File(tempDir, "task.xml");
        ByteArrayOutputStream bao = new ByteArrayOutputStream();
        bao.write(IO.BOM.UTF16LE.getBOM());
        bao.write(xml.getBytes("UTF-16LE"));
        IO.writeToFile(xmlFile, bao.toByteArray());
        String taskName = "AppWorkRunAsLocalSystem_" + UniqueAlltimeID.next();
        activeLocalSystemTasks.add(taskName);
        File exitCodeFile = new File(tempDir, "exitcode.txt");
        try {
            WindowsTaskSchedulerCom.createTaskFromXml(taskName, xml);
            WindowsTaskSchedulerCom.runTask(taskName);
            File stdoutFile = new File(tempDir, "stdout.txt");
            File stderrFile = new File(tempDir, "stderr.txt");
            File pidFile = new File(tempDir, "pid.txt");
            final String prefix = "runAsLocalSystem";
            LineHandler stdoutLineHandler = new LineHandler() {
                @Override
                public void handleLine(String line, Object caller) {
                    if (line != null && line.trim().length() > 0) {
                        LogV3.info(prefix + " [stdout]: " + line);
                    }
                }
            };
            LineHandler stderrLineHandler = new LineHandler() {
                @Override
                public void handleLine(String line, Object caller) {
                    if (line != null && line.trim().length() > 0) {
                        LogV3.warning(prefix + " [stderr]: " + line);
                    }
                }
            };
            ContinuesFileLineReader readerStdout = new ContinuesFileLineReader(stdoutLineHandler, stdoutFile.getAbsolutePath(), UTF8).run();
            ContinuesFileLineReader readerStderr = new ContinuesFileLineReader(stderrLineHandler, stderrFile.getAbsolutePath(), UTF8).run();
            long deadline = System.currentTimeMillis() + 300000;
            boolean pidFound = false;
            while (!exitCodeFile.isFile() && System.currentTimeMillis() < deadline) {
                if (!pidFound && pidFile.isFile()) {
                    Thread.sleep(500);
                    try {
                        String pidStr = IO.readFileToString(pidFile).trim();
                        int pid = Integer.parseInt(pidStr, 10);
                        LogV3.info("LocalSystem task PID:" + pid);
                        if (pid > 0 && !activeLocalSystemPids.contains(Integer.valueOf(pid))) {
                            activeLocalSystemPids.add(Integer.valueOf(pid));
                        }
                        pidFound = true;
                        callback.sendResponse(new Response(AdminHelperProcess.PROCESS_STARTED, String.valueOf(pid)));
                        AdminHelperProcess.registerRequestIdPid(requestId, pid);
                        if (resultHexDir == null && !waitFor) {
                            callback.sendResponse(new Response(AdminHelperProcess.CMD_RESULT, "-999\n\n\n"));
                            return;
                        }
                    } catch (Throwable t) {
                    }
                }
                Thread.sleep(200);
            }
            if (!exitCodeFile.isFile()) {
                throw new Exception("runAsLocalSystem (JNA) timed out waiting for task (5 min)");
            }
            Thread.sleep(100);
            try {
                readerStdout.closeAndFlush();
            } catch (Throwable t) {
                LogV3.log(t);
            }
            try {
                readerStderr.closeAndFlush();
            } catch (Throwable t) {
                LogV3.log(t);
            }
            int exitCode = 0;
            try {
                String ec = IO.readFileToString(exitCodeFile).trim();
                exitCode = Integer.parseInt(ec, 10);
            } catch (Throwable t) {
            }
            String stdout = "";
            String stderr = "";
            if (resultHexDir != null && resultHexDir.isDirectory()) {
                File resultHexFile = new File(resultHexDir, "result.hex");
                String resultHex = "";
                if (resultHexFile.isFile()) {
                    resultHex = IO.readFileToString(resultHexFile).trim();
                }
                if (resultHex.length() == 0 && stdoutFile.isFile()) {
                    throw new Exception("runAsLocalSystem (JNA) task did not produce result.hex. exitCode=" + exitCode);
                }
                String jvmOut = "";
                String jvmErr = "";
                if (stdoutFile.isFile()) {
                    jvmOut = new String(IO.readStream(-1, new FileInputStream(stdoutFile)), UTF8);
                }
                if (stderrFile.isFile()) {
                    jvmErr = new String(IO.readStream(-1, new FileInputStream(stderrFile)), UTF8);
                }
                stdout = AdminHelperProcess.mergeTaskResultHexWithProcessStreams(resultHex, jvmOut, jvmErr);
            } else {
                if (stdoutFile.isFile()) {
                    stdout = new String(IO.readStream(-1, new FileInputStream(stdoutFile)), UTF8);
                }
                if (stderrFile.isFile()) {
                    stderr = new String(IO.readStream(-1, new FileInputStream(stderrFile)), UTF8);
                }
            }
            if (resultHexDir != null) {
                callback.sendResponse(new Response(AdminHelperProcess.TASK_RESULT, stdout));
            } else {
                String msg = exitCode + "\n" + HexFormatter.byteArrayToHex(stdout.getBytes(UTF8)) + "\n" + HexFormatter.byteArrayToHex(stderr.getBytes(UTF8));
                callback.sendResponse(new Response(AdminHelperProcess.CMD_RESULT, msg));
            }
        } finally {
            activeLocalSystemTasks.remove(taskName);
            try {
                WindowsTaskSchedulerCom.deleteTask(taskName);
            } catch (Throwable t) {
                LogV3.info("JNA Task Scheduler delete failed for " + taskName + ": " + t.getMessage());
            }
            xmlFile.delete();
            new File(tempDir, "stdout.txt").delete();
            new File(tempDir, "stderr.txt").delete();
            exitCodeFile.delete();
            new File(tempDir, "pid.txt").delete();
            if (resultHexDir != null && resultHexDir.equals(tempDir)) {
                new File(tempDir, "result.hex").delete();
            }
            if (workDir == null || !workDir.isDirectory()) {
                tempDir.delete();
            }
        }
    }

    private static File getRunAsLocalSystemTempDir() {
        String systemRoot = System.getenv("SystemRoot");
        if (systemRoot == null || systemRoot.trim().length() == 0) {
            systemRoot = "C:\\Windows";
        }
        File systemTempRoot = new File(systemRoot.trim(), "Temp");
        return new File(systemTempRoot, "AppWorkRunAsLocalSystem_" + UniqueAlltimeID.next());
    }

    /**
     * Minimal COM wrapper for Task Scheduler 2.0: create task from XML, run, delete.
     */
    private static final class WindowsTaskSchedulerCom {
        private static final String CLSID_TASKSCHEDULER  = "{0F87369F-A4E5-4CFC-BD3E-73E6154572DD}";
        private static final String IID_ITASKSERVICE     = "{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}";
        private static final int    CLSCTX_INPROC_SERVER = 1;
        private static final int    VTABLE_CONNECT       = 7;
        private static final int    VTABLE_GET_FOLDER    = 8;
        private static final int    VTABLE_REGISTER_TASK = 14;
        private static final int    VTABLE_GET_TASK      = 15;
        private static final int    VTABLE_DELETE_TASK   = 16;
        private static final int    VTABLE_RUN           = 11;

        static void createTaskFromXml(String taskName, String xml) throws Exception {
            Pointer pService = createServiceAndConnect();
            try {
                PointerByReference pFolderRef = new PointerByReference();
                callGetFolder(pService, pFolderRef);
                Pointer pFolder = pFolderRef.getValue();
                if (pFolder == null) {
                    throw new Exception("GetFolder returned null");
                }
                try {
                    registerTask(pFolder, taskName, xml);
                } finally {
                    release(pFolder);
                }
            } finally {
                release(pService);
            }
        }

        static void runTask(String taskName) throws Exception {
            Pointer pService = createServiceAndConnect();
            try {
                PointerByReference pFolderRef = new PointerByReference();
                callGetFolder(pService, pFolderRef);
                Pointer pFolder = pFolderRef.getValue();
                if (pFolder == null) {
                    throw new Exception("GetFolder returned null");
                }
                try {
                    PointerByReference pTaskRef = new PointerByReference();
                    getTask(pFolder, taskName, pTaskRef);
                    Pointer pTask = pTaskRef.getValue();
                    if (pTask != null) {
                        try {
                            callRun(pTask);
                        } finally {
                            release(pTask);
                        }
                    } else {
                        throw new Exception("GetTask returned null for " + taskName);
                    }
                } finally {
                    release(pFolder);
                }
            } finally {
                release(pService);
            }
        }

        static void deleteTask(String taskName) throws Exception {
            Pointer pService = createServiceAndConnect();
            try {
                PointerByReference pFolderRef = new PointerByReference();
                callGetFolder(pService, pFolderRef);
                Pointer pFolder = pFolderRef.getValue();
                if (pFolder == null) {
                    throw new Exception("GetFolder returned null");
                }
                try {
                    deleteTask(pFolder, taskName);
                } finally {
                    release(pFolder);
                }
            } finally {
                release(pService);
            }
        }

        private static Pointer createServiceAndConnect() throws Exception {
            Ole32 ole32 = Ole32.INSTANCE;
            ole32.CoInitializeEx(null, Ole32.COINIT_MULTITHREADED);
            GUID clsid = new GUID(CLSID_TASKSCHEDULER);
            GUID iid = new GUID(IID_ITASKSERVICE);
            PointerByReference ppv = new PointerByReference();
            HRESULT hr = ole32.CoCreateInstance(clsid, null, CLSCTX_INPROC_SERVER, iid, ppv);
            if (hr.intValue() != 0) {
                throw new Exception("CoCreateInstance(TaskScheduler) failed: 0x" + Integer.toHexString(hr.intValue()));
            }
            Pointer pService = ppv.getValue();
            connect(pService);
            return pService;
        }

        private static void connect(Pointer pService) throws Exception {
            Pointer vtable = pService.getPointer(0);
            Function connectFn = Function.getFunction(vtable.getPointer(VTABLE_CONNECT * Native.POINTER_SIZE), Function.ALT_CONVENTION);
            VARIANT empty = new VARIANT();
            empty.setValue(Variant.VT_EMPTY, null);
            Object result = connectFn.invoke(int.class, new Object[] { pService, empty, empty, empty, empty });
            if (result != null && ((Integer) result).intValue() != 0) {
                throw new Exception("ITaskService.Connect failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
            }
        }

        private static void callGetFolder(Pointer pService, PointerByReference ppFolder) throws Exception {
            OleAuto oleAuto = OleAuto.INSTANCE;
            BSTR pathBstr = oleAuto.SysAllocString("\\");
            try {
                Pointer vtable = pService.getPointer(0);
                Function getFolderFn = Function.getFunction(vtable.getPointer(VTABLE_GET_FOLDER * Native.POINTER_SIZE), Function.ALT_CONVENTION);
                Object result = getFolderFn.invoke(int.class, new Object[] { pService, pathBstr, ppFolder });
                if (result != null && ((Integer) result).intValue() != 0) {
                    throw new Exception("ITaskService.GetFolder failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
                }
            } finally {
                oleAuto.SysFreeString(pathBstr);
            }
        }

        private static void registerTask(Pointer pFolder, String taskName, String xml) throws Exception {
            OleAuto oleAuto = OleAuto.INSTANCE;
            BSTR nameBstr = oleAuto.SysAllocString(taskName);
            BSTR xmlBstr = oleAuto.SysAllocString(xml);
            try {
                VARIANT empty = new VARIANT();
                empty.setValue(Variant.VT_EMPTY, null);
                PointerByReference ppTask = new PointerByReference();
                Pointer vtable = pFolder.getPointer(0);
                Function registerFn = Function.getFunction(vtable.getPointer(VTABLE_REGISTER_TASK * Native.POINTER_SIZE), Function.ALT_CONVENTION);
                Object result = registerFn.invoke(int.class, new Object[] { pFolder, nameBstr, xmlBstr, Integer.valueOf(TASK_CREATE_OR_UPDATE), empty, empty, Integer.valueOf(TASK_LOGON_NONE), empty, ppTask });
                if (result != null && ((Integer) result).intValue() != 0) {
                    throw new Exception("ITaskFolder.RegisterTask failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
                }
                Pointer pTask = ppTask.getValue();
                if (pTask != null) {
                    release(pTask);
                }
            } finally {
                oleAuto.SysFreeString(nameBstr);
                oleAuto.SysFreeString(xmlBstr);
            }
        }

        private static void getTask(Pointer pFolder, String taskName, PointerByReference ppTask) throws Exception {
            OleAuto oleAuto = OleAuto.INSTANCE;
            BSTR nameBstr = oleAuto.SysAllocString(taskName);
            try {
                Pointer vtable = pFolder.getPointer(0);
                Function getTaskFn = Function.getFunction(vtable.getPointer(VTABLE_GET_TASK * Native.POINTER_SIZE), Function.ALT_CONVENTION);
                Object result = getTaskFn.invoke(int.class, new Object[] { pFolder, nameBstr, ppTask });
                if (result != null && ((Integer) result).intValue() != 0) {
                    throw new Exception("ITaskFolder.GetTask failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
                }
            } finally {
                oleAuto.SysFreeString(nameBstr);
            }
        }

        private static void callRun(Pointer pTask) throws Exception {
            VARIANT empty = new VARIANT();
            empty.setValue(Variant.VT_EMPTY, null);
            Pointer vtable = pTask.getPointer(0);
            Function runFn = Function.getFunction(vtable.getPointer(VTABLE_RUN * Native.POINTER_SIZE), Function.ALT_CONVENTION);
            Object result = runFn.invoke(int.class, new Object[] { pTask, empty });
            if (result != null && ((Integer) result).intValue() != 0) {
                throw new Exception("IRegisteredTask.Run failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
            }
        }

        private static void deleteTask(Pointer pFolder, String taskName) throws Exception {
            OleAuto oleAuto = OleAuto.INSTANCE;
            BSTR nameBstr = oleAuto.SysAllocString(taskName);
            try {
                Pointer vtable = pFolder.getPointer(0);
                Function deleteFn = Function.getFunction(vtable.getPointer(VTABLE_DELETE_TASK * Native.POINTER_SIZE), Function.ALT_CONVENTION);
                Object result = deleteFn.invoke(int.class, new Object[] { pFolder, nameBstr, Integer.valueOf(TASK_DELETE_FLAG) });
                if (result != null && ((Integer) result).intValue() != 0) {
                    throw new Exception("ITaskFolder.DeleteTask failed: 0x" + Integer.toHexString(((Integer) result).intValue()));
                }
            } finally {
                oleAuto.SysFreeString(nameBstr);
            }
        }

        private static void release(Pointer p) {
            if (p == null) {
                return;
            }
            try {
                Pointer vtable = p.getPointer(0);
                Function releaseFn = Function.getFunction(vtable.getPointer(2 * Native.POINTER_SIZE), Function.ALT_CONVENTION);
                releaseFn.invoke(int.class, new Object[] { p });
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }
}
