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

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.WindowsUtils.AccessPermissionEntry;
import org.appwork.utils.os.WindowsUtils.SID;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.ContinuesFileLineReader;
import org.appwork.utils.processes.LineHandler;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;
import org.appwork.utils.processes.command.ProcessErrorStream;
import org.appwork.utils.processes.command.ProcessInputStream;
import org.appwork.utils.processes.command.ProcessStreamReader;
import org.appwork.utils.singleapp.FailedToSendResponseException;
import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.Response;
import org.appwork.utils.singleapp.ResponseSender;
import org.appwork.utils.singleapp.SingleAppInstance;
import org.appwork.utils.swing.dialog.Dialog;

/**
 * Helper process started elevated (UAC). Accepts connections only from processes in its parent chain (direct parent and ancestors) via
 * SingleAppInstance; runs commands and responds with CMD_RESULT. Exits when the direct parent process is no longer alive. Used by
 * {@link AdminExecuter#runAsAdmin}.
 * <p>
 * Command line: {@code -lockDir <path>} (required); {@code -appRoot <path>} or {@code -channelId <id>} (one required, for clientID match);
 * {@code -publicKeyBase64 <base64>} or env {@link HelperChannelCrypto#ENV_PUBLIC_KEY_BASE64} to enable encrypted responses (symmetric AES;
 * client's RSA public key used for key exchange).
 * <p>
 * Protocol (SingleAppInstance): client sends RUN_CMD, JSON array of command strings, workDir path, options JSON (waitFor, keepRunning);
 * server responds PROCESS_STARTED(pid) then CMD_RESULT. message = exitCode + "\n" + hex(stdout) + "\n" + hex(stderr).
 */
public final class AdminHelperProcess {
    public static final String                           APP_ID                         = "AWTestAdminHelper";
    public static final String                           RUN_CMD                        = "RUN_CMD";
    public static final String                           CMD_RESULT                     = "CMD_RESULT";
    public static final String                           RUN_TASK                       = "RUN_TASK";
    public static final String                           TASK_RESULT                    = "TASK_RESULT";
    /**
     * Sent by helper as soon as the child process is started; message = PID (decimal string). Client sets options.setTaskPID and
     * options.setTerminateAction so options.terminate() can terminate.
     */
    public static final String                           PROCESS_STARTED                = "PROCESS_STARTED";
    /** Client sends KILL_PID with message[1]=pid (decimal string). Helper terminates that process. */
    public static final String                           KILL_PID                       = "KILL_PID";
    /**
     * Client sends TERMINATE_REQUEST_ID with message[1]=requestId (UniqueAlltimeID). Helper terminates the process associated with that
     * request.
     */
    public static final String                           TERMINATE_REQUEST_ID           = "TERMINATE_REQUEST_ID";
    /** Run a command as NT AUTHORITY\SYSTEM via schtasks; helper owns temp dir and logs, responds with CMD_RESULT. */
    public static final String                           RUN_AS_LOCAL_SYSTEM_CMD        = "RUN_AS_LOCAL_SYSTEM_CMD";
    /** Run an ElevatedTestTask as NT AUTHORITY\SYSTEM via schtasks; helper owns temp dir and logs, responds with TASK_RESULT. */
    public static final String                           RUN_AS_LOCAL_SYSTEM_TASK       = "RUN_AS_LOCAL_SYSTEM_TASK";
    /** Response type for encrypted payload (message = Base64 of symmetric AES cipher; key exchange via RSA). */
    public static final String                           ENCRYPTED                      = "ENCRYPTED";
    private static final Charset                         UTF8                           = Charset.forName("UTF-8");
    /** Delay in ms before exiting after parent process is gone (time to log and terminate child processes). */
    private static final long                            PARENT_GONE_EXIT_DELAY_MS      = 20000;
    /** Set by main() after SingleAppInstance started; used by shutdown hook to call exit(). */
    private static volatile AdminHelperSingleAppInstance singleInstanceRef              = null;
    /** Optional debug window for shutdown/parent-gone logs; null if not shown. */
    private static volatile JTextArea                    debugWindowTextArea            = null;
    /** Task names of active run-as-LocalSystem schtasks (for cleanup on exit). */
    private static final Set<String>                     activeLocalSystemTasks         = java.util.Collections.synchronizedSet(new HashSet<String>());
    /** PIDs of Local System processes we started (from pid.txt), for cleanup on exit. */
    private static final List<Integer>                   activeLocalSystemPids          = java.util.Collections.synchronizedList(new ArrayList<Integer>());
    /** Request ID (UniqueAlltimeID) to PID map for terminate-by-request-id. */
    private static final Map<String, Integer>            requestIdToPid                 = new ConcurrentHashMap<String, Integer>();
    /**
     * When true, run-as-LocalSystem uses the JNA Task Scheduler COM API instead of schtasks.exe. Requires JNA; falls back to schtasks if
     * JNA is unavailable or COM fails.
     */
    private static final boolean                         USE_JNA_FOR_LOCAL_SYSTEM_TASKS = true;
    private static List<Integer>                         parentChainPids;
    private static boolean                               showDebugWindow;

    public static void main(String[] args) {
        if (!CrossSystem.isWindows()) {
            System.err.println("AdminHelperProcess is Windows-only");
            exit(1);
        }
        parentChainPids = buildParentChainPids();
        if (parentChainPids == null || parentChainPids.size() == 0) {
            Dialog.I().showMessageDialog("Cannot detect Parent PIDS");
            exit(-1);
        }
        String lockDirPath = null;
        String appRootPath = null;
        String channelId = null;
        String publicKeyBase64 = null;
        String debugLogDirPath = null;
        boolean verbose = false;
        for (int i = 0; i < args.length - 1; i++) {
            if ("-lockDir".equals(args[i]) && args[i + 1] != null) {
                lockDirPath = args[i + 1];
            } else if ("-appRoot".equals(args[i]) && args[i + 1] != null) {
                appRootPath = args[i + 1];
            } else if ("-channelId".equals(args[i]) && args[i + 1] != null) {
                channelId = args[i + 1];
            } else if ("-publicKeyBase64".equals(args[i]) && args[i + 1] != null) {
                publicKeyBase64 = args[i + 1];
            } else if ("-debugLogDir".equals(args[i]) && args[i + 1] != null) {
                debugLogDirPath = args[i + 1];
            }
        }
        if (publicKeyBase64 == null || publicKeyBase64.length() == 0) {
            publicKeyBase64 = System.getenv(HelperChannelCrypto.ENV_PUBLIC_KEY_BASE64);
        }
        for (int i = 0; i < args.length; i++) {
            if ("-verbose".equals(args[i])) {
                verbose = true;
            } else if ("-showDebugWindow".equals(args[i])) {
                showDebugWindow = true;
            }
        }
        // When debug window is requested, create it first so it is visible immediately; helper will exit only when user closes the window.
        if (showDebugWindow) {
            try {
                SwingUtilities.invokeAndWait(new Runnable() {
                    @Override
                    public void run() {
                        JFrame frame = new JFrame("AdminHelperProcess Debug");
                        JTextArea ta = new JTextArea(20, 60);
                        ta.setEditable(false);
                        frame.getContentPane().add(new JScrollPane(ta));
                        frame.pack();
                        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
                        frame.addWindowListener(new WindowAdapter() {
                            @Override
                            public void windowClosing(WindowEvent e) {
                                LogV3.info("Request to exit (debug window closed). Terminating child processes...");
                                new Thread(new Runnable() {
                                    @Override
                                    public void run() {
                                        exit(0);
                                    }
                                }, "AdminHelper-ExitOnClose").start();
                            }
                        });
                        frame.setVisible(true);
                        debugWindowTextArea = ta;
                    }
                });
            } catch (Throwable t) {
                String reason = t.getClass().getSimpleName();
                if (t.getMessage() != null) {
                    reason += ": " + t.getMessage();
                }
                if (t instanceof java.awt.HeadlessException) {
                    reason = "No display available (headless). Debug window only works when the helper runs in an interactive session with a display.";
                }
                System.err.println("[AdminHelperProcess] Could not create debug window - " + reason);
                System.err.flush();
                showDebugWindow = false;
            }
        }
        if (showDebugWindow) {
            addLoggerSinkToDebugWindow();
            teeStdoutStderrToDebugWindow();
        }
        if (debugLogDirPath != null && debugLogDirPath.length() > 0) {
            teeStdoutStderrToFiles(new File(debugLogDirPath));
        }
        if (lockDirPath == null || lockDirPath.length() == 0) {
            System.err.println("Missing or empty -lockDir");
            exit(1);
        }
        // idRoot for SingleAppInstance: prefer random channel ID (allows parallel tests to use same helper); else app root
        String idRoot = (channelId != null && channelId.trim().length() > 0) ? channelId.trim() : appRootPath;
        if (idRoot == null || idRoot.length() == 0) {
            System.err.println("Missing or empty -appRoot or -channelId");
            exit(1);
        }
        java.security.PublicKey clientPublicKey = null;
        if (publicKeyBase64 != null && publicKeyBase64.length() > 0) {
            try {
                clientPublicKey = HelperChannelCrypto.decodePublicKeyFromBase64(publicKeyBase64);
            } catch (Throwable t) {
                System.err.println("Failed to decode public key (responses will be unencrypted): " + t.getMessage());
            }
        }
        if (verbose) {
            System.out.println("[AdminHelper verbose] main args.length=" + args.length + " lockDir=" + lockDirPath + " idRoot=" + (channelId != null ? "channelId" : "appRoot"));
            System.out.flush();
        }
        LogV3.info("Desktop Support: " + CrossSystem.getDesktopSupport().getClass().getName());
        LogV3.info("Process Handler: " + ProcessHandlerFactory.getProcessHandler().getClass().getName());
        final boolean verboseLog = verbose;
        final boolean useDebugWindowExit = showDebugWindow;
        Thread parentWatch = new Thread(new Runnable() {
            @Override
            public void run() {
                if (useDebugWindowExit) {
                    LogV3.info("Parent watch: watching ancestor PIDs " + parentChainPids + ". Helper will exit only when you close this window.");
                }
                while (ProcessHandlerFactory.getProcessHandler().isProcessesAlive(new ProcessInfo(parentChainPids.get(0)))) {
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        break;
                    }
                }
                LogV3.info("Parent PID gone");
                if (useDebugWindowExit) {
                    LogV3.info("Close this window to terminate the helper (child processes will be terminated on exit).");
                    return;
                }
                LogV3.info("Exiting now.");
                exit(0);
            }
        });
        parentWatch.setDaemon(true);
        parentWatch.setName("AdminHelper-ParentWatch");
        parentWatch.start();
        final File directory = new File(lockDirPath);
        final java.security.PublicKey encryptKey = clientPublicKey;
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(final ResponseSender callback, String[] message) throws Exception {
                final ResponseSender callbackToUse;
                if (encryptKey != null) {
                    callbackToUse = new ResponseSender() {
                        @Override
                        public void sendResponse(Response response) throws org.appwork.utils.singleapp.FailedToSendResponseException {
                            try {
                                String type = response.getType();
                                String msg = response.getMessage() != null ? response.getMessage() : "";
                                byte[] payload = (type + "\n" + msg).getBytes(UTF8);
                                byte[] encrypted = HelperChannelCrypto.encryptWithPublicKey(encryptKey, payload);
                                String base64 = Base64.encodeToString(encrypted, false);
                                callback.sendResponse(new Response(ENCRYPTED, base64));
                            } catch (Throwable t) {
                                Exception e = t instanceof Exception ? (Exception) t : new Exception(t);
                                throw new org.appwork.utils.singleapp.FailedToSendResponseException(null, e);
                            }
                        }
                    };
                } else {
                    callbackToUse = callback;
                }
                if (verboseLog) {
                    System.out.println("[AdminHelper verbose] onIncommingMessage message=" + (message == null ? "null" : "len=" + message.length + " [0]=" + (message.length > 0 ? message[0] : "") + " [1]len=" + (message.length > 1 && message[1] != null ? message[1].length() : 0) + " [2]=" + (message.length > 2 ? message[2] : "")));
                    System.out.flush();
                }
                if (message == null || message.length < 1) {
                    callbackToUse.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Expected RUN_CMD or RUN_TASK"));
                    return;
                }
                if (RUN_TASK.equals(message[0])) {
                    handleRunTask(callbackToUse, message, verboseLog);
                    return;
                }
                if (RUN_AS_LOCAL_SYSTEM_CMD.equals(message[0]) && message.length >= 2) {
                    handleRunAsLocalSystemCmd(callbackToUse, message, verboseLog);
                    return;
                }
                if (RUN_AS_LOCAL_SYSTEM_TASK.equals(message[0]) && message.length >= 3) {
                    handleRunAsLocalSystemTask(callbackToUse, message, verboseLog);
                    return;
                }
                if (RUN_CMD.equals(message[0]) && message.length >= 2) {
                    handleRunCmd(callbackToUse, message, verboseLog);
                    return;
                }
                if (KILL_PID.equals(message[0]) && message.length >= 2) {
                    handleKillPid(callbackToUse, message);
                    return;
                }
                if (TERMINATE_REQUEST_ID.equals(message[0]) && message.length >= 2) {
                    handleTerminateRequestId(callbackToUse, message);
                    return;
                }
                callbackToUse.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Expected RUN_CMD and command JSON or RUN_TASK and serialized task"));
            }
        };
        AdminHelperSingleAppInstance single = new AdminHelperSingleAppInstance(APP_ID, directory, listener, idRoot);
        single.setAllowedRemotePIDs(parentChainPids);
        // Fail-closed: do not start if we cannot restrict connections to the parent chain (would otherwise accept all)
        boolean hasAllowedPids = (parentChainPids != null && !parentChainPids.isEmpty()) || getDirectParentPid() > 0;
        if (!hasAllowedPids) {
            System.err.println("AdminHelperProcess: cannot determine parent chain or direct parent PID; refusing to start (security: would accept all connections)");
            exit(1);
        }
        try {
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] SingleAppInstance.start() directory=" + directory.getAbsolutePath());
                System.out.flush();
            }
            single.start();
            singleInstanceRef = single;
        } catch (Throwable t) {
            if (verboseLog) {
                t.printStackTrace(System.out);
                System.out.flush();
            }
            t.printStackTrace();
            exit(1);
        }
        if (verboseLog) {
            System.out.println("[AdminHelper verbose] SingleAppInstance started, entering sleep loop");
            System.out.flush();
        }
        try {
            while (true) {
                Thread.sleep(60000);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        exit(2);
    }

    /**
     * @param i
     */
    private static void exit(int i) {
        LogV3.info("Shutdown hook: terminating child processes...");
        terminateDescendantProcesses();
        AdminHelperSingleAppInstance ref = singleInstanceRef;
        if (ref != null) {
            Thread exitThread = ref.exit(true);
            if (exitThread != null) {
                try {
                    exitThread.join(3000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }
        LogV3.info("Shutdown hook finished.");
        if (showDebugWindow) {
            LogV3.info("Exit in 3s");
            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                LogV3.log(e);
            }
        }
        System.exit(i);
    }

    /**
     * Handles RUN_TASK: run task in a separate JRE process. message[1]=hex task, optional message[2]=options JSON. Sends
     * PROCESS_STARTED(pid) when the subprocess is started. waitFor true: wait for process to end, then send TASK_RESULT with real return
     * value and exit code. waitFor false: send empty TASK_RESULT immediately and return (caller does not wait). keepRunning: process is
     * left running after task completes (e.g. server); return value is still returned when waitFor is true. No effect when waitFor is
     * false. Uses {@link RunTaskAsSystemMain} with a temp dir.
     */
    private static void handleRunTask(ResponseSender callback, String[] message, final boolean verboseLog) throws Exception {
        if (message.length < 2 || message[1] == null) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "RUN_TASK requires hex-encoded serialized task in message[1]"));
            return;
        }
        String hexTask = message[1];
        boolean waitFor = true;
        boolean keepRunning = false;
        String requestId = null;
        if (message.length >= 3 && message[2] != null && message[2].trim().length() > 0) {
            waitFor = parseHelperOptionsWaitFor(message[2]);
            keepRunning = parseHelperOptionsKeepRunning(message[2]);
            requestId = parseHelperOptionsRequestId(message[2]);
        }
        LogV3.info("[AdminHelper] RUN_TASK received optionsJsonLen=" + (message.length >= 3 && message[2] != null ? message[2].length() : 0) + " waitFor=" + waitFor + " keepRunning=" + keepRunning + " requestId=" + (requestId != null ? requestId : "null"));
        byte[] taskBytes;
        try {
            taskBytes = HexFormatter.hexToByteArray(hexTask);
        } catch (Throwable t) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Invalid hex in RUN_TASK: " + t.getMessage()));
            return;
        }
        File tempDir = new File(System.getProperty("java.io.tmpdir"), "AppWorkRunTask_" + UniqueAlltimeID.next());
        tempDir.mkdirs();
        File taskFile = new File(tempDir, "task.bin");
        try {
            IO.writeToFile(taskFile, taskBytes);
            if (keepRunning) {
                IO.writeToFile(new File(tempDir, "keep_running"), "true".getBytes(UTF8));
            }
            String taskId = tempDir.getName().startsWith("AppWorkRunTask_") ? tempDir.getName().substring("AppWorkRunTask_".length()) : tempDir.getName();
            String javaBin = CrossSystem.getJavaBinary();
            String classpath = System.getProperty("java.class.path");
            String[] cmd = new String[] { javaBin, "-Dfile.encoding=UTF-8", "-cp", classpath, RunTaskAsSystemMain.class.getName(), tempDir.getAbsolutePath(), taskId };
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] RUN_TASK: starting subprocess java ... RunTaskAsSystemMain " + tempDir.getAbsolutePath() + " waitFor=" + waitFor + " keepRunning=" + keepRunning);
                System.out.flush();
            }
            java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(cmd);
            pb.directory(tempDir);
            Process process = pb.start();
            int pid = getPidFromProcess(process);
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] RUN_TASK subprocess started pid=" + pid);
                System.out.flush();
            }
            callback.sendResponse(new Response(PROCESS_STARTED, String.valueOf(pid)));
            registerRequestIdPid(requestId, pid);
            LogV3.info("[AdminHelper] RUN_TASK sent PROCESS_STARTED pid=" + pid);
            if (!waitFor) {
                LogV3.info("[AdminHelper] RUN_TASK waitFor=false -> sending empty TASK_RESULT and returning (caller will not wait for result)");
                byte[] emptyResult = serializeResult(new AdminTaskResultWrapper(null, "", ""));
                String hexEmpty = emptyResult != null && emptyResult.length > 0 ? HexFormatter.byteArrayToHex(emptyResult) : "";
                callback.sendResponse(new Response(TASK_RESULT, hexEmpty));
                return;
            }
            LogV3.info("[AdminHelper] RUN_TASK waitFor=true -> waiting for subprocess pid=" + pid + " to exit...");
            try {
                process.getOutputStream().close();
            } catch (Throwable t) {
            }
            ByteArrayOutputStream stdoutCollect = new ByteArrayOutputStream();
            ByteArrayOutputStream stderrCollect = new ByteArrayOutputStream();
            ProcessStreamReader stdReader = new ProcessStreamReader("AdminHelper-RUN_TASK-stdout", process, new ProcessInputStream(process), stdoutCollect);
            ProcessStreamReader errReader = new ProcessStreamReader("AdminHelper-RUN_TASK-stderr", process, new ProcessErrorStream(process), stderrCollect);
            if (CrossSystem.isWindows()) {
                stdReader.setPriority(Thread.NORM_PRIORITY + 1);
                errReader.setPriority(Thread.NORM_PRIORITY + 1);
            }
            stdReader.start();
            errReader.start();
            File resultHexFile = new File(tempDir, "result.hex");
            if (keepRunning) {
                long deadline = System.currentTimeMillis() + 120000;
                while (!resultHexFile.isFile() && System.currentTimeMillis() < deadline) {
                    Thread.sleep(200);
                }
                if (!resultHexFile.isFile()) {
                    callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Task subprocess (keepRunning) did not produce result.hex within 120s"));
                    return;
                }
                Thread.sleep(300);
                String processStdout = new String(stdoutCollect.toByteArray(), UTF8);
                String processStderr = new String(stderrCollect.toByteArray(), UTF8);
                String hexResult = IO.readFileToString(resultHexFile).trim();
                if (hexResult.length() == 0) {
                    callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Task subprocess produced empty result.hex"));
                    return;
                }
                byte[] resultBytes = HexFormatter.hexToByteArray(hexResult);
                AdminTaskResultWrapper wrapper = deserializeResultWrapper(resultBytes);
                AdminTaskResultWrapper merged = new AdminTaskResultWrapper(wrapper.getReturnValue(), processStdout != null ? processStdout : "", processStderr != null ? processStderr : "");
                byte[] mergedBytes = serializeResult(merged);
                String hexMerged = mergedBytes != null && mergedBytes.length > 0 ? HexFormatter.byteArrayToHex(mergedBytes) : "";
                LogV3.info("[AdminHelper] RUN_TASK keepRunning=true: result.hex present, sending TASK_RESULT (subprocess pid=" + pid + " stays alive)");
                callback.sendResponse(new Response(TASK_RESULT, hexMerged));
                return;
            }
            int exitCode = process.waitFor();
            LogV3.info("[AdminHelper] RUN_TASK subprocess pid=" + pid + " exited exitCode=" + exitCode);
            stdReader.waitFor();
            errReader.waitFor();
            String processStdout = new String(stdoutCollect.toByteArray(), UTF8);
            String processStderr = new String(stderrCollect.toByteArray(), UTF8);
            if (exitCode != 0) {
                String err = processStderr != null ? processStderr : "";
                callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Task subprocess exit code " + exitCode + (err.length() > 0 ? "\n" + err : "")));
                return;
            }
            if (!resultHexFile.isFile()) {
                callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Task subprocess did not produce result.hex"));
                return;
            }
            String hexResult = IO.readFileToString(resultHexFile).trim();
            if (hexResult.length() == 0) {
                callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Task subprocess produced empty result.hex"));
                return;
            }
            byte[] resultBytes = HexFormatter.hexToByteArray(hexResult);
            AdminTaskResultWrapper wrapper = deserializeResultWrapper(resultBytes);
            AdminTaskResultWrapper merged = new AdminTaskResultWrapper(wrapper.getReturnValue(), processStdout != null ? processStdout : "", processStderr != null ? processStderr : "");
            byte[] mergedBytes = serializeResult(merged);
            String hexMerged = mergedBytes != null && mergedBytes.length > 0 ? HexFormatter.byteArrayToHex(mergedBytes) : "";
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] RUN_TASK subprocess finished, resultLen=" + (mergedBytes != null ? mergedBytes.length : 0));
                System.out.flush();
            }
            LogV3.info("[AdminHelper] RUN_TASK sending TASK_RESULT resultLen=" + (mergedBytes != null ? mergedBytes.length : 0) + " (keepRunning=" + keepRunning + ")");
            callback.sendResponse(new Response(TASK_RESULT, hexMerged));
        } catch (Throwable t) {
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] RUN_TASK exception: " + t.getClass().getName() + " " + t.getMessage());
                System.out.flush();
            }
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, Exceptions.getStackTrace(t)));
        } finally {
            try {
                taskFile.delete();
            } catch (Throwable t) {
            }
            if (!keepRunning) {
                try {
                    new File(tempDir, "result.hex").delete();
                } catch (Throwable t) {
                }
                try {
                    tempDir.delete();
                } catch (Throwable t) {
                }
            }
        }
    }

    private static AdminTaskResultWrapper deserializeResultWrapper(byte[] bytes) throws IOException, ClassNotFoundException {
        ObjectInputStream ois = null;
        try {
            ois = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (AdminTaskResultWrapper) ois.readObject();
        } finally {
            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                }
            }
        }
    }

    private static ElevatedTestTask deserializeTask(byte[] bytes) throws IOException, ClassNotFoundException {
        ObjectInputStream ois = null;
        try {
            ois = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (ElevatedTestTask) ois.readObject();
        } finally {
            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                }
            }
        }
    }

    private static byte[] serializeResult(Serializable result) throws IOException {
        if (result == null) {
            return new byte[0];
        }
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = null;
        try {
            oos = new ObjectOutputStream(baos);
            oos.writeObject(result);
            oos.flush();
            return baos.toByteArray();
        } finally {
            if (oos != null) {
                try {
                    oos.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Handles RUN_CMD: start command as a separate process (not in helper). Sends PROCESS_STARTED with PID as soon as the process is
     * started. waitFor true: wait for process to end and send CMD_RESULT with exit code and output. waitFor false: send CMD_RESULT
     * immediately (-999) and return so the caller continues without waiting. keepRunning has no effect for CMD invocation.
     */
    private static void handleRunCmd(ResponseSender callback, String[] message, final boolean verboseLog) throws Exception {
        String json = message[1];
        List<String> cmdList = Deser.fromString(json, TypeRef.STRING_LIST);
        if (cmdList == null || cmdList.isEmpty()) {
            if (verboseLog) {
                System.out.println("[AdminHelper verbose] reject: empty command");
                System.out.flush();
            }
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Empty command"));
            return;
        }
        String[] cmd = cmdList.toArray(new String[cmdList.size()]);
        String workDirPath = message.length >= 3 && message[2] != null ? message[2] : "";
        File workDir = (workDirPath.length() > 0) ? new File(workDirPath) : null;
        boolean waitFor = true;
        boolean keepRunning = false;
        String requestId = null;
        if (message.length >= 4 && message[3] != null && message[3].trim().length() > 0) {
            waitFor = parseHelperOptionsWaitFor(message[3]);
            keepRunning = parseHelperOptionsKeepRunning(message[3]);
            requestId = parseHelperOptionsRequestId(message[3]);
        }
        LogV3.info("[AdminHelper] RUN_CMD received waitFor=" + waitFor + " keepRunning=" + keepRunning + " (keepRunning has no effect for CMD)");
        if (verboseLog) {
            System.out.println("[AdminHelper verbose] running cmd=" + java.util.Arrays.toString(cmd) + " workDir=" + workDir + " waitFor=" + waitFor + " keepRunning=" + keepRunning);
            System.out.flush();
        }
        java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(cmd);
        if (workDir != null && workDir.isDirectory()) {
            pb.directory(workDir);
        }
        Process process = pb.start();
        int pid = getPidFromProcess(process);
        if (verboseLog) {
            System.out.println("[AdminHelper verbose] process started pid=" + pid);
            System.out.flush();
        }
        callback.sendResponse(new Response(PROCESS_STARTED, String.valueOf(pid)));
        registerRequestIdPid(requestId, pid);
        LogV3.info("[AdminHelper] RUN_CMD sent PROCESS_STARTED pid=" + pid);
        if (!waitFor) {
            LogV3.info("[AdminHelper] RUN_CMD waitFor=false -> sending CMD_RESULT -999 and returning");
            callback.sendResponse(new Response(CMD_RESULT, "-999\n\n\n"));
            return;
        }
        LogV3.info("[AdminHelper] RUN_CMD waitFor=true -> waiting for process pid=" + pid + " to exit...");
        try {
            process.getOutputStream().close();
        } catch (Throwable t) {
        }
        ByteArrayOutputStream stdoutCollect = new ByteArrayOutputStream();
        ByteArrayOutputStream stderrCollect = new ByteArrayOutputStream();
        ProcessStreamReader stdReader = new ProcessStreamReader("AdminHelper-RUN_CMD-stdout", process, new ProcessInputStream(process), stdoutCollect);
        ProcessStreamReader errReader = new ProcessStreamReader("AdminHelper-RUN_CMD-stderr", process, new ProcessErrorStream(process), stderrCollect);
        if (CrossSystem.isWindows()) {
            stdReader.setPriority(Thread.NORM_PRIORITY + 1);
            errReader.setPriority(Thread.NORM_PRIORITY + 1);
        }
        stdReader.start();
        errReader.start();
        int exitCode = process.waitFor();
        stdReader.waitFor();
        errReader.waitFor();
        LogV3.info("[AdminHelper] RUN_CMD process pid=" + pid + " exited exitCode=" + exitCode);
        String stdout = new String(stdoutCollect.toByteArray(), UTF8);
        String stderr = new String(stderrCollect.toByteArray(), UTF8);
        if (verboseLog) {
            System.out.println("[AdminHelper verbose] command done exitCode=" + exitCode + " stdoutLen=" + stdout.length() + " stderrLen=" + stderr.length());
            System.out.flush();
        }
        String msg = exitCode + "\n" + HexFormatter.byteArrayToHex(stdout.getBytes(UTF8)) + "\n" + HexFormatter.byteArrayToHex(stderr.getBytes(UTF8));
        LogV3.info("[AdminHelper] RUN_CMD sending CMD_RESULT exitCode=" + exitCode);
        callback.sendResponse(new Response(CMD_RESULT, msg));
    }

    /**
     * Returns PID of the given process. Uses Process.pid() when available (Java 9+), else on Windows uses handle + GetProcessId. Returns -1
     * if PID could not be determined.
     */
    private static int getPidFromProcess(Process process) {
        if (process == null) {
            return -1;
        }
        try {
            if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
                Long pid = ReflectionUtils.invoke(process.getClass(), "pid", process, Long.class);
                if (pid != null && pid.longValue() > 0) {
                    return pid.intValue();
                }
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
        if (CrossSystem.isWindows() && org.appwork.JNAHelper.isJNAAvailable()) {
            try {
                Long handle = ReflectionUtils.getFieldValue(process.getClass(), "handle", process, Long.class);
                if (handle != null && handle.longValue() != 0) {
                    long pid = getPidFromWindowsHandle(handle.longValue());
                    if (pid > 0) {
                        return (int) pid;
                    }
                }
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
        return -1;
    }

    private static long getPidFromWindowsHandle(long handle) {
        try {
            com.sun.jna.platform.win32.WinNT.HANDLE hProcess = new com.sun.jna.platform.win32.WinNT.HANDLE(com.sun.jna.Pointer.createConstant(handle));
            return com.sun.jna.platform.win32.Kernel32.INSTANCE.GetProcessId(hProcess);
        } catch (Throwable t) {
            return -1;
        }
    }

    /**
     * Registers a request ID to PID mapping so that TERMINATE_REQUEST_ID can terminate the process. Called when PROCESS_STARTED is sent.
     * Package-visible for {@link RunAsLocalSystemJNA}.
     */
    static void registerRequestIdPid(String requestId, int pid) {
        if (requestId != null && requestId.length() > 0 && pid > 0) {
            requestIdToPid.put(requestId, Integer.valueOf(pid));
        }
    }

    /**
     * Handles KILL_PID: message[1]=pid (decimal string). Terminates that process and sends CMD_RESULT "0" on success.
     */
    private static void handleKillPid(ResponseSender callback, String[] message) throws FailedToSendResponseException {
        if (message.length < 2 || message[1] == null) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "KILL_PID requires message[1]=pid"));
            return;
        }
        int pid = parsePid(message[1]);
        if (pid <= 0) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "KILL_PID invalid pid: " + message[1]));
            return;
        }
        try {
            ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
            List<ProcessInfo> list = handler.listByProcessInfo(new ProcessInfo(pid));
            if (list != null && !list.isEmpty()) {
                handler.terminateForced(list.get(0), -1);
                activeLocalSystemPids.remove(Integer.valueOf(pid));
                callback.sendResponse(new Response(CMD_RESULT, "0\n\n\n"));
            } else {
                callback.sendResponse(new Response(CMD_RESULT, "0\n\n\n"));
            }
        } catch (Throwable t) {
            LogV3.info("KILL_PID failed for " + pid + ": " + t.getMessage());
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, Exceptions.getStackTrace(t)));
        }
    }

    /**
     * Handles TERMINATE_REQUEST_ID: message[1]=requestId (UniqueAlltimeID). Terminates the process associated with that request and sends
     * CMD_RESULT "0" on success.
     */
    private static void handleTerminateRequestId(ResponseSender callback, String[] message) throws FailedToSendResponseException {
        if (message.length < 2 || message[1] == null) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "TERMINATE_REQUEST_ID requires message[1]=requestId"));
            return;
        }
        String requestId = message[1].trim();
        if (requestId.length() == 0) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "TERMINATE_REQUEST_ID empty requestId"));
            return;
        }
        Integer pidBox = requestIdToPid.remove(requestId);
        if (pidBox == null || pidBox.intValue() <= 0) {
            callback.sendResponse(new Response(CMD_RESULT, "0\n\n\n"));
            return;
        }
        int pid = pidBox.intValue();
        try {
            ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
            List<ProcessInfo> list = handler.listByProcessInfo(new ProcessInfo(pid));
            if (list != null && !list.isEmpty()) {
                handler.terminateForced(list.get(0), -1);
                activeLocalSystemPids.remove(pidBox);
            }
            callback.sendResponse(new Response(CMD_RESULT, "0\n\n\n"));
        } catch (Throwable t) {
            LogV3.info("TERMINATE_REQUEST_ID failed for requestId=" + requestId + " pid=" + pid + ": " + t.getMessage());
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, Exceptions.getStackTrace(t)));
        }
    }

    /**
     * Ends all tracked schtasks (run-as-LocalSystem) and terminates tracked PIDs. Call before terminateDescendantProcesses on exit.
     */
    private static int endTrackedLocalSystemTasksAndKillPids() {
        String[] taskNames;
        int count = 0;
        synchronized (activeLocalSystemTasks) {
            taskNames = activeLocalSystemTasks.toArray(new String[activeLocalSystemTasks.size()]);
        }
        for (String taskName : taskNames) {
            try {
                LogV3.info("End Local System task: " + taskName);
                LogV3.info(ProcessBuilderFactory.runCommand("schtasks", "/end", "/tn", taskName) + "");
                count++;
            } catch (Throwable t) {
                LogV3.info("Failed to end task " + taskName + ": " + t.getMessage());
            }
        }
        Integer[] pids;
        synchronized (activeLocalSystemPids) {
            pids = activeLocalSystemPids.toArray(new Integer[activeLocalSystemPids.size()]);
        }
        for (Integer pid : pids) {
            if (pid != null && pid.intValue() > 0) {
                try {
                    LogV3.info("LOcal system PID to kill:  " + pid);
                    List<ProcessInfo> pis = ProcessHandlerFactory.getProcessHandler().listByProcessInfo(new ProcessInfo(pid));
                    if (pis.size() > 0) {
                        LogV3.info("Try to kill " + pis.get(0));
                        ProcessHandlerFactory.getProcessHandler().terminateForced(pis.get(0), -1);
                        count++;
                        LogV3.info("Terminated Local System PID: " + pid);
                    }
                } catch (Throwable t) {
                    LogV3.info("Failed to terminate PID " + pid + ": " + t.getMessage());
                }
            }
        }
        return count;
    }

    /**
     * RUN_AS_LOCAL_SYSTEM_CMD: message[1]=workDir path, message[2]=JSON cmd array, message[3]=options JSON (waitFor, keepRunning). Sends
     * PROCESS_STARTED(pid). waitFor true: wait and send CMD_RESULT with real output; false: send CMD_RESULT immediately and return.
     * keepRunning has no effect for CMD. Uses JNA or schtasks according to {@link #USE_JNA_FOR_LOCAL_SYSTEM_TASKS}.
     */
    private static void handleRunAsLocalSystemCmd(ResponseSender callback, String[] message, final boolean verboseLog) throws Exception {
        String workDirPath = message.length >= 2 && message[1] != null ? message[1] : "";
        String jsonCmd = message.length >= 3 && message[2] != null ? message[2] : "[]";
        boolean waitFor = true;
        boolean keepRunning = false;
        String requestId = null;
        if (message.length >= 4 && message[3] != null && message[3].trim().length() > 0) {
            waitFor = parseHelperOptionsWaitFor(message[3]);
            keepRunning = parseHelperOptionsKeepRunning(message[3]);
            requestId = parseHelperOptionsRequestId(message[3]);
        }
        LogV3.info("[AdminHelper] RUN_AS_LOCAL_SYSTEM_CMD received waitFor=" + waitFor + " keepRunning=" + keepRunning);
        List<String> cmdList = Deser.fromString(jsonCmd, TypeRef.STRING_LIST);
        if (cmdList == null || cmdList.isEmpty()) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "RUN_AS_LOCAL_SYSTEM_CMD requires non-empty command JSON in message[2]"));
            return;
        }
        String[] cmd = cmdList.toArray(new String[cmdList.size()]);
        File workDir = (workDirPath.length() > 0) ? new File(workDirPath) : null;
        if (USE_JNA_FOR_LOCAL_SYSTEM_TASKS && org.appwork.JNAHelper.isJNAAvailable()) {
            try {
                RunAsLocalSystemJNA.runAsLocalSystemViaJna(callback, workDir, cmd, waitFor, keepRunning, requestId, null, null, verboseLog, activeLocalSystemTasks, activeLocalSystemPids);
                return;
            } catch (Throwable t) {
                LogV3.warning("RunAsLocalSystem JNA failed, falling back to schtasks: " + t.getMessage());
            }
        }
        runAsLocalSystemViaSchtasks(callback, workDir, cmd, waitFor, keepRunning, requestId, null, null, verboseLog);
    }

    /**
     * RUN_AS_LOCAL_SYSTEM_TASK: message[1]=hex serialized task, message[2]=classpath. Writes task.bin, runs RunTaskAsSystemMain, responds
     * with TASK_RESULT. Uses JNA or schtasks according to {@link #USE_JNA_FOR_LOCAL_SYSTEM_TASKS}.
     */
    private static void handleRunAsLocalSystemTask(ResponseSender callback, String[] message, final boolean verboseLog) throws Exception {
        LogV3.info("[AdminHelper] RUN_AS_LOCAL_SYSTEM_TASK received (always wait for TASK_RESULT)");
        if (message.length < 3 || message[1] == null || message[2] == null) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "RUN_AS_LOCAL_SYSTEM_TASK requires message[1]=hex task, message[2]=classpath"));
            return;
        }
        byte[] taskBytes;
        try {
            taskBytes = HexFormatter.hexToByteArray(message[1]);
        } catch (Throwable t) {
            callback.sendResponse(new Response(SingleAppInstance.EXCEPTION, "Invalid hex in RUN_AS_LOCAL_SYSTEM_TASK: " + t.getMessage()));
            return;
        }
        String classpath = message[2];
        File tempDir = getRunAsLocalSystemTempDir();
        tempDir.mkdirs();
        restrictTempDirToAdminAndSystem(tempDir);
        File taskFile = new File(tempDir, "task.bin");
        try {
            IO.writeToFile(taskFile, taskBytes);
            String taskId = tempDir.getName().startsWith("AppWorkRunAsLocalSystem_") ? tempDir.getName().substring("AppWorkRunAsLocalSystem_".length()) : tempDir.getName();
            String javaBin = CrossSystem.getJavaBinary();
            String[] cmd = new String[] { javaBin, "-Dfile.encoding=UTF-8", "-cp", classpath, RunTaskAsSystemMain.class.getName(), tempDir.getAbsolutePath(), taskId };
            if (USE_JNA_FOR_LOCAL_SYSTEM_TASKS && org.appwork.JNAHelper.isJNAAvailable()) {
                try {
                    RunAsLocalSystemJNA.runAsLocalSystemViaJna(callback, tempDir, cmd, true, false, null, tempDir, taskFile, verboseLog, activeLocalSystemTasks, activeLocalSystemPids);
                    return;
                } catch (Throwable t) {
                    LogV3.warning("RunAsLocalSystem JNA failed, falling back to schtasks: " + t.getMessage());
                }
            }
            runAsLocalSystemViaSchtasks(callback, tempDir, cmd, true, false, null, tempDir, taskFile, verboseLog);
        } finally {
            taskFile.delete();
            new File(tempDir, "result.hex").delete();
            tempDir.delete();
        }
    }

    /**
     * Creates temp dir for run-as-LocalSystem (writable by SYSTEM). Helper runs elevated so can use SystemRoot\Temp.
     */
    private static File getRunAsLocalSystemTempDir() {
        String systemRoot = System.getenv("SystemRoot");
        if (systemRoot == null || systemRoot.trim().length() == 0) {
            systemRoot = "C:\\Windows";
        }
        File systemTempRoot = new File(systemRoot.trim(), "Temp");
        return new File(systemTempRoot, "AppWorkRunAsLocalSystem_" + UniqueAlltimeID.next());
    }

    /**
     * Restricts the given directory so that only LocalSystem (NT AUTHORITY\SYSTEM) and Built-in Administrators can read/write. Removes
     * inherited ACEs and sets DACL to these two principals with full control and inheritance so that files created in the dir get the same
     * protection. Used for run-as-LocalSystem temp dir and its files. Package-visible for {@link RunAsLocalSystemJNA}.
     *
     * @param tempDir
     *            existing directory to restrict (must exist)
     * @throws Exception
     *             if ACL could not be set (e.g. JNA and icacls fallback both failed)
     */
    static void restrictTempDirToAdminAndSystem(File tempDir) throws Exception {
        if (tempDir == null || !tempDir.isDirectory()) {
            return;
        }
        try {
            WindowsUtils.applyPermissions(tempDir, false, true, AccessPermissionEntry.allow(SID.SID_LOCAL_SYSTEM.sid, WindowsUtils.PERMISSIONSET_FULL).containerInherit(true).objectInherit(true), AccessPermissionEntry.allow(SID.SID_BUILTIN_ADMINISTRATORS.sid, WindowsUtils.PERMISSIONSET_FULL).containerInherit(true).objectInherit(true));
        } catch (RuntimeException e) {
            LogV3.info("restrictTempDirToAdminAndSystem: WindowsUtils.applyPermissions failed, trying icacls fallback: " + e.getMessage());
            String path = tempDir.getAbsolutePath();
            String icacls = new File(System.getenv("SystemRoot") != null ? System.getenv("SystemRoot") : "C:\\Windows", "system32\\icacls.exe").getAbsolutePath();
            ProcessOutput out = ProcessBuilderFactory.runCommand(icacls, path, "/inheritance:r", "/grant:r", "*S-1-5-18:(OI)(CI)F", "/grant:r", "*S-1-5-32-544:(OI)(CI)F");
            if (out.getExitCode() != 0) {
                throw new Exception("Failed to restrict temp dir ACL (JNA and icacls): " + out.getErrOutString());
            }
        }
    }

    /**
     * Runs command as Local System via schtasks. Creates temp dir (or uses workDir when provided for TASK). Builds XML, schtasks
     * create/run, polls stdout/stderr/exitcode, forwards lines to log, tracks task name and pid.txt PID for cleanup. For TASK mode,
     * resultHexDir and taskBinFile are set and result.hex is read. When waitFor is true, waits and sends CMD_RESULT or TASK_RESULT with
     * real output; when waitFor is false and CMD (resultHexDir null), sends CMD_RESULT immediately after PROCESS_STARTED and returns.
     */
    private static void runAsLocalSystemViaSchtasks(ResponseSender callback, File workDir, String[] cmd, boolean waitFor, boolean keepRunning, String requestId, File resultHexDir, File taskBinFile, boolean verboseLog) throws Exception {
        File tempDir = (workDir != null && workDir.isDirectory()) ? workDir : getRunAsLocalSystemTempDir();
        if (workDir == null || !workDir.isDirectory()) {
            tempDir.mkdirs();
        }
        restrictTempDirToAdminAndSystem(tempDir);
        String workDirPath = (workDir != null && workDir.isDirectory()) ? workDir.getAbsolutePath() : tempDir.getAbsolutePath();
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        if (!waitFor) {
            LogV3.info("Execute as Local System: " + commandLine);
            commandLine = "start /b \"\" " + commandLine;
        } else {
            LogV3.info("Execute and wait as Local System: " + commandLine);
        }
        String tempPath = tempDir.getAbsolutePath().replace("\"", "\"\"");
        String wrapperArgs = "/c \"chcp 65001 >nul && cd /d \"" + workDirPath.replace("\"", "\"\"") + "\" && " + commandLine + " > \"" + tempPath + "\\stdout.txt\" 2> \"" + tempPath + "\\stderr.txt\" & echo %ERRORLEVEL% > \"" + tempPath + "\\exitcode.txt\"\"";
        String xml = WindowsUtils.buildTaskXmlAsLocalSystem("cmd.exe", wrapperArgs, tempDir.getAbsolutePath());
        File xmlFile = new File(tempDir, "task.xml");
        java.io.ByteArrayOutputStream bao = new java.io.ByteArrayOutputStream();
        bao.write(IO.BOM.UTF16LE.getBOM());
        bao.write(xml.getBytes("UTF-16LE"));
        IO.writeToFile(xmlFile, bao.toByteArray());
        String taskName = "AppWorkRunAsLocalSystem_" + UniqueAlltimeID.next();
        activeLocalSystemTasks.add(taskName);
        File exitCodeFile = new File(tempDir, "exitcode.txt");
        try {
            ProcessOutput createOut = ProcessBuilderFactory.runCommand("schtasks", "/create", "/tn", taskName, "/xml", xmlFile.getAbsolutePath(), "/f");
            if (createOut.getExitCode() != 0) {
                throw new Exception("schtasks create failed: " + createOut.getErrOutString());
            }
            ProcessOutput runOut = ProcessBuilderFactory.runCommand("schtasks", "/run", "/tn", taskName);
            if (runOut.getExitCode() != 0) {
                throw new Exception("schtasks run failed: " + runOut.getErrOutString());
            }
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
                    Thread.sleep(500); // allow file to be fully written
                    try {
                        String pidStr = IO.readFileToString(pidFile).trim();
                        int pid = Integer.parseInt(pidStr, 10);
                        LogV3.info("LocalSystem task PID:" + pid);
                        if (pid > 0 && !activeLocalSystemPids.contains(Integer.valueOf(pid))) {
                            activeLocalSystemPids.add(Integer.valueOf(pid));
                        }
                        pidFound = true;
                        callback.sendResponse(new Response(PROCESS_STARTED, String.valueOf(pid)));
                        registerRequestIdPid(requestId, pid);
                        if (resultHexDir == null && !waitFor) {
                            LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks CMD waitFor=false -> sending CMD_RESULT -999 and returning");
                            callback.sendResponse(new Response(CMD_RESULT, "-999\n\n\n"));
                            return;
                        }
                        if (resultHexDir == null) {
                            LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks CMD waitFor=true -> waiting for exitcode.txt (pid=" + pid + ")...");
                        }
                    } catch (Throwable t) {
                    }
                }
                Thread.sleep(200);
            }
            if (!exitCodeFile.isFile()) {
                throw new Exception("runAsLocalSystem timed out waiting for task (5 min)");
            }
            LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks task finished (exitcode.txt present), reading exitCode and sending result");
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
                if (resultHexFile.isFile()) {
                    stdout = IO.readFileToString(resultHexFile).trim();
                }
                if (stdout.length() == 0) {
                    String debugDirList = "";
                    try {
                        String[] names = resultHexDir.list();
                        debugDirList = names != null ? Arrays.asList(names).toString() : "null";
                    } catch (Throwable t) {
                        debugDirList = "list failed: " + t.getMessage();
                    }
                    String stdoutPrefix = "";
                    String stderrPrefix = "";
                    try {
                        if (stdoutFile.isFile()) {
                            byte[] buf = IO.readStream(1024, new java.io.FileInputStream(stdoutFile));
                            stdoutPrefix = buf != null ? new String(buf, UTF8).replace("\r", " ").replace("\n", " ") : "";
                        }
                    } catch (Throwable t) {
                        stdoutPrefix = "(read failed: " + t.getMessage() + ")";
                    }
                    try {
                        if (stderrFile.isFile()) {
                            byte[] buf = IO.readStream(1024, new java.io.FileInputStream(stderrFile));
                            stderrPrefix = buf != null ? new String(buf, UTF8).replace("\r", " ").replace("\n", " ") : "";
                        }
                    } catch (Throwable t) {
                        stderrPrefix = "(read failed: " + t.getMessage() + ")";
                    }
                    LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks result.hex missing: resultHexDir=" + resultHexDir.getAbsolutePath() + " files=" + debugDirList + " exitCode=" + exitCode);
                    LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks task stdout.txt prefix: " + stdoutPrefix);
                    LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks task stderr.txt prefix: " + stderrPrefix);
                    throw new Exception("runAsLocalSystem task did not produce result.hex. exitCode=" + exitCode + " resultHexDir=" + resultHexDir.getAbsolutePath() + " files=" + debugDirList + " stdoutPrefix=" + stdoutPrefix + " stderrPrefix=" + stderrPrefix);
                }
            } else {
                if (stdoutFile.isFile()) {
                    stdout = new String(IO.readStream(-1, new java.io.FileInputStream(stdoutFile)), UTF8);
                }
                if (stderrFile.isFile()) {
                    stderr = new String(IO.readStream(-1, new java.io.FileInputStream(stderrFile)), UTF8);
                }
            }
            if (resultHexDir != null) {
                LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks sending TASK_RESULT (task mode)");
                callback.sendResponse(new Response(TASK_RESULT, stdout));
            } else {
                LogV3.info("[AdminHelper] runAsLocalSystemViaSchtasks sending CMD_RESULT exitCode=" + exitCode + " (CMD mode)");
                String msg = exitCode + "\n" + HexFormatter.byteArrayToHex(stdout.getBytes(UTF8)) + "\n" + HexFormatter.byteArrayToHex(stderr.getBytes(UTF8));
                callback.sendResponse(new Response(CMD_RESULT, msg));
            }
        } finally {
            activeLocalSystemTasks.remove(taskName);
            try {
                ProcessBuilderFactory.runCommand("schtasks", "/delete", "/tn", taskName, "/f");
            } catch (Throwable t) {
                LogV3.info("schtasks delete failed for " + taskName + ": " + t.getMessage());
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

    /**
     * Adds a log sink to the current LogV3 factory so that all log output (info, warning, exception, etc.) is also appended to the debug
     * window. Only has an effect when the factory is a SimpleLoggerFactory (e.g. PreInitLoggerFactory).
     */
    private static void addLoggerSinkToDebugWindow() {
        final JTextArea ta = debugWindowTextArea;
        if (ta == null) {
            return;
        }
        try {
            org.appwork.loggingv3.LogV3Factory f = org.appwork.loggingv3.LogV3.getFactory();
            if (f instanceof org.appwork.loggingv3.simple.SimpleLoggerFactory) {
                ((org.appwork.loggingv3.simple.SimpleLoggerFactory) f).addSink(new LogToDebugWindowSink(ta));
            }
        } catch (Throwable t) {
            System.err.println("[AdminHelperProcess] Could not add logger sink to debug window: " + t.getMessage());
        }
    }

    /**
     * Wraps current System.out and System.err so that all output is also appended to the debug window. Call after the debug window is
     * created.
     */
    private static void teeStdoutStderrToDebugWindow() {
        final JTextArea ta = debugWindowTextArea;
        if (ta == null) {
            return;
        }
        PrintStream out = System.out;
        PrintStream err = System.err;
        System.setOut(new TeePrintStream(out, ta, false));
        System.setErr(new TeePrintStream(err, ta, true));
    }

    /**
     * Tees current System.out and System.err to stdout.txt and stderr.txt in the given directory (UTF-8, append). Used so the parent
     * process can read and display helper logs. Call after the debug window tee if both are used.
     */
    private static void teeStdoutStderrToFiles(File debugLogDir) {
        if (debugLogDir == null || !debugLogDir.isDirectory()) {
            return;
        }
        try {
            File outFile = new File(debugLogDir, "stdout.txt");
            File errFile = new File(debugLogDir, "stderr.txt");
            PrintStream fileOut = new PrintStream(new FileOutputStream(outFile), true, "UTF-8");
            PrintStream fileErr = new PrintStream(new FileOutputStream(errFile), true, "UTF-8");
            System.setOut(new TeeToSecondStream(System.out, fileOut));
            System.setErr(new TeeToSecondStream(System.err, fileErr));
        } catch (Throwable t) {
            System.err.println("[AdminHelperProcess] Could not tee to debugLogDir: " + t.getMessage());
        }
    }

    /**
     * PrintStream that forwards all output to a delegate and to a second PrintStream (e.g. file).
     */
    private static final class TeeToSecondStream extends PrintStream {
        private final PrintStream second;

        TeeToSecondStream(PrintStream delegate, PrintStream second) {
            super(delegate, true);
            this.second = second;
        }

        @Override
        public void write(int b) {
            super.write(b);
            second.write(b);
        }

        @Override
        public void write(byte[] buf, int off, int len) {
            super.write(buf, off, len);
            second.write(buf, off, len);
        }

        @Override
        public void flush() {
            super.flush();
            second.flush();
        }
    }

    /**
     * PrintStream that writes to an underlying stream and appends the same output to a JTextArea (on EDT).
     */
    private static final class TeePrintStream extends PrintStream {
        private final JTextArea textArea;
        private final boolean   isStderr;

        TeePrintStream(PrintStream delegate, JTextArea textArea, boolean isStderr) {
            super(delegate, true);
            this.textArea = textArea;
            this.isStderr = isStderr;
        }

        @Override
        public void write(int b) {
            super.write(b);
            appendToWindow(String.valueOf((char) b));
        }

        @Override
        public void write(byte[] buf, int off, int len) {
            super.write(buf, off, len);
            if (len > 0) {
                appendToWindow(new String(buf, off, len, UTF8));
            }
        }

        private void appendToWindow(final String s) {
            if (s == null || s.length() == 0) {
                return;
            }
            final String prefix = isStderr ? "[stderr] " : "";
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    textArea.append(prefix + s);
                    textArea.setCaretPosition(textArea.getDocument().getLength());
                }
            });
        }
    }

    /**
     * Returns the PID of the direct parent of the current process, or -1 if unknown. Used for the parent watch thread (exit when parent
     * exits).
     */
    private static int getDirectParentPid() {
        try {
            long myPid = CrossSystem.getPID();
            if (myPid <= 0 || myPid > Integer.MAX_VALUE) {
                return -1;
            }
            List<ProcessInfo> list = ProcessHandlerFactory.getProcessHandler().listByPids((int) myPid);
            if (list == null || list.isEmpty()) {
                return -1;
            }
            return list.get(0).getParentPid();
        } catch (Throwable e) {
            return -1;
        }
    }

    private static List<Integer> buildParentChainPids() {
        try {
            long myPid = CrossSystem.getPID();
            if (myPid <= 0 || myPid > Integer.MAX_VALUE) {
                return null;
            }
            List<Integer> chain = new ArrayList<Integer>();
            int pid = (int) myPid;
            int maxSteps = 15;
            while (pid > 0 && maxSteps-- > 0) {
                List<ProcessInfo> list = ProcessHandlerFactory.getProcessHandler().listByPids(pid);
                if (list == null || list.size() == 0) {
                    break;
                }
                if ("eclipse.exe".equals(list.get(0).getExecutableName())) {
                    break;
                }
                int parent = (list != null && !list.isEmpty()) ? list.get(0).getParentPid() : -1;
                chain.add(parent);
                if (parent <= 0 || parent == pid) {
                    break;
                }
                pid = parent;
            }
            return chain;
        } catch (Throwable e) {
            return null;
        }
    }

    /**
     * Returns PIDs of all descendant processes (children, grandchildren, etc.) of the current process. Excludes the current process. Tries
     * ProcessHandler.listByPath(null) first; on Windows, on failure or empty result falls back to WMIC to find children (e.g. when JNA is
     * not available in the elevated process).
     */
    private static Set<Integer> getDescendantPids() {
        long myPid = CrossSystem.getPID();
        if (myPid <= 0 || myPid > Integer.MAX_VALUE) {
            return new HashSet<Integer>();
        }
        int myPidInt = (int) myPid;
        return getDescendantPidsViaHandler(myPidInt);
    }

    /**
     * Uses ProcessHandler.listByPath(null) to get all processes and build descendant set. Returns null on throw or if handler unsupported.
     */
    private static Set<Integer> getDescendantPidsViaHandler(int myPid) {
        try {
            ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
            List<ProcessInfo> all = handler.listByPath(null);
            if (all == null || all.isEmpty()) {
                return new HashSet<Integer>();
            }
            Set<Integer> tree = new HashSet<Integer>();
            tree.add(Integer.valueOf(myPid));
            boolean changed = true;
            while (changed) {
                changed = false;
                for (ProcessInfo p : all) {
                    int pid = p.getPid();
                    int parent = p.getParentPid();
                    if (parent >= 0 && tree.contains(Integer.valueOf(parent)) && !tree.contains(Integer.valueOf(pid))) {
                        tree.add(Integer.valueOf(pid));
                        changed = true;
                    }
                }
            }
            tree.remove(Integer.valueOf(myPid));
            return tree;
        } catch (Throwable e) {
            LogV3.info("getDescendantPids via ProcessHandler failed: " + e.getClass().getSimpleName() + " " + e.getMessage());
            return null;
        }
    }

    /**
     * Parses the options JSON sent by the client (from {@link ProcessOptions#toHelperPayloadJson()}). Returns waitFor flag; default true
     * when missing or invalid. Accepts Boolean, String "true"/"false", or Number (1 = true, 0 = false).
     */
    private static boolean parseHelperOptionsWaitFor(String optionsJson) {
        try {
            Map<String, Object> map = Deser.fromString(optionsJson, TypeRef.MAP);
            if (map != null && map.containsKey("waitFor")) {
                Object v = map.get("waitFor");
                if (v instanceof Boolean) {
                    return ((Boolean) v).booleanValue();
                }
                if (v instanceof String) {
                    return Boolean.parseBoolean(((String) v).trim());
                }
                if (v instanceof Number) {
                    return ((Number) v).intValue() != 0;
                }
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
        return true;
    }

    /**
     * Parses the options JSON sent by the client. Returns keepRunning flag; default false when missing or invalid.
     */
    private static boolean parseHelperOptionsKeepRunning(String optionsJson) {
        try {
            Map<String, Object> map = Deser.fromString(optionsJson, TypeRef.MAP);
            if (map != null && map.containsKey("keepRunning")) {
                Object v = map.get("keepRunning");
                if (v instanceof Boolean) {
                    return ((Boolean) v).booleanValue();
                }
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
        return false;
    }

    /**
     * Parses the options JSON sent by the client. Returns requestId (UniqueAlltimeID) or null when missing or invalid.
     */
    private static String parseHelperOptionsRequestId(String optionsJson) {
        try {
            Map<String, Object> map = Deser.fromString(optionsJson, TypeRef.MAP);
            if (map != null && map.containsKey("requestId")) {
                Object v = map.get("requestId");
                if (v != null) {
                    String s = v.toString();
                    if (s != null && s.trim().length() > 0) {
                        return s.trim();
                    }
                }
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
        return null;
    }

    private static int parsePid(String s) {
        if (s == null) {
            return -1;
        }
        s = s.trim();
        if (s.length() == 0) {
            return -1;
        }
        try {
            return Integer.parseInt(s, 10);
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Terminates all descendant processes (children and their descendants). Returns the number of processes that were successfully
     * terminated. Uses ProcessHandler.terminateForced; on failure (e.g. NotSupported) falls back to taskkill /f /pid.
     */
    private static int terminateDescendantProcesses() {
        Set<Integer> pids = getDescendantPids();
        int count = 0;
        count += endTrackedLocalSystemTasksAndKillPids();
        try {
            ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
            for (Integer pid : pids) {
                try {
                    ProcessInfo pi = new ProcessInfo(pid.intValue());
                    if (!handler.isProcessesAlive(pi)) {
                        continue;
                    }
                    LogV3.info("Terminate " + pi);
                    if (handler.terminateForced(pi, 0)) {
                        count++;
                    }
                } catch (Throwable t) {
                    LogV3.info("Failed to terminate PID " + pid + ": " + t.getMessage());
                }
            }
        } catch (Throwable t) {
            LogV3.info("Error terminating descendant processes: " + t.getMessage());
        }
        return count;
    }
}