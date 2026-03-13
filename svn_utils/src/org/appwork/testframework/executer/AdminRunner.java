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
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.Deser;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.processes.ProcessOutput;
import org.appwork.utils.singleapp.ExceptionInRunningInstance;
import org.appwork.utils.singleapp.Response;
import org.appwork.utils.singleapp.ResponseListener;
import org.appwork.utils.singleapp.SingleAppInstance;
import org.appwork.utils.singleapp.SingleAppInstance.ErrorReadingResponseException;
import org.appwork.utils.singleapp.SingleAppInstance.InvalidResponseID;
import org.appwork.utils.singleapp.SingleAppInstance.NoPortFileException;

/**
 * Used by {@link AdminExecuter#runAsAdmin(File, String[], ProcessOptions)} to send RUN_CMD to the helper and get {@link ProcessOutput}.
 * Tasks run via {@link AdminExecuter#runAsAdmin(ElevatedTestTask, org.appwork.storage.TypeRef)} execute in the helper process directly and
 * do not use this class.
 */
public final class AdminRunner implements AdminRunnerApi {
    private static final Charset    UTF8                = Charset.forName("UTF-8");
    private static final int        RESPONSE_TIMEOUT_MS = 120000;
    private final SingleAppInstance client;

    AdminRunner(SingleAppInstance client) {
        this.client = client;
    }

    /**
     * Run a command in the elevated helper process.
     *
     * @param command
     *            command and arguments (e.g. "schtasks", "/create", ...)
     * @return process output (exit code, stdout, stderr)
     * @throws Exception
     *             if communication fails or the helper reported an exception
     */
    public ProcessOutput runCommand(String[] command) throws Exception {
        return runCommand((java.io.File) null, Arrays.asList(command));
    }

    /**
     * Run a command in the elevated helper process with the given working directory.
     */
    public ProcessOutput runCommand(java.io.File workDir, String[] command) throws Exception {
        return runCommand(workDir, Arrays.asList(command));
    }

    /**
     * Run a command in the elevated helper process (no working directory).
     */
    public ProcessOutput runCommand(List<String> command) throws Exception {
        return runCommand((java.io.File) null, command);
    }

    /**
     * Run a command in the elevated helper process with the given working directory.
     *
     * @param workDir
     *            working directory for the process (null = default)
     * @param command
     *            command and arguments as list
     * @return process output (exit code, stdout, stderr)
     */
    public ProcessOutput runCommand(java.io.File workDir, List<String> command) throws Exception {
        return runCommand(workDir, command, null);
    }

    /**
     * Run a command in the elevated helper process with the given working directory and options (e.g. log callback).
     *
     * @param workDir
     *            working directory for the process (null = default)
     * @param command
     *            command and arguments as list
     * @param options
     *            optional process options (waitFor, logCallback); null = default
     * @return process output (exit code, stdout, stderr)
     */
    public ProcessOutput runCommand(java.io.File workDir, List<String> command, final ProcessOptions options) throws Exception {
        final String json = Deser.toString(command);
        final String workDirPath = (workDir != null && workDir.isDirectory()) ? workDir.getAbsolutePath() : "";
        LogV3.info("runAsAdmin: sending command to helper: " + command);
        if (AdminExecuter.isVerbose()) {
            LogV3.info("AdminRunner [verbose]: runCommand workDir=" + workDir + " workDirPath=" + workDirPath + " jsonLen=" + (json != null ? json.length() : 0) + " command=" + command);
        }
        final CountDownLatch latch = new CountDownLatch(1);
        final AtomicReference<ProcessOutput> result = new AtomicReference<ProcessOutput>(null);
        final AtomicReference<Exception> error = new AtomicReference<Exception>(null);
        ResponseListener listener = new ResponseListener() {
            @Override
            public void onConnected(SingleAppInstance single, java.net.SocketAddress remoteSocket, String[] message) {
                if (AdminExecuter.isVerbose()) {
                    LogV3.info("AdminRunner [verbose]: onConnected remote=" + remoteSocket);
                }
            }

            @Override
            public void onReceivedResponse(SingleAppInstance single, Response response) {
                if (AdminExecuter.isVerbose()) {
                    LogV3.info("AdminRunner [verbose]: onReceivedResponse type=" + (response != null ? response.getType() : "null") + " messageLen=" + (response != null && response.getMessage() != null ? response.getMessage().length() : 0));
                }
                if (AdminHelperProcess.PROCESS_STARTED.equals(response.getType())) {
                    String pidStr = response.getMessage();
                    if (pidStr != null && pidStr.trim().length() > 0 && options != null) {
                        try {
                            int pid = Integer.parseInt(pidStr.trim(), 10);
                            options.setTaskPID(Integer.valueOf(pid));
                            final String reqId = options.getRequestId();
                            if (reqId != null) {
                                final SingleAppInstance clientRef = client;
                                options.setTerminateAction(new Runnable() {
                                    @Override
                                    public void run() {
                                        try {
                                            clientRef.sendToRunningInstance(null, AdminHelperProcess.TERMINATE_REQUEST_ID, reqId);
                                        } catch (Throwable t) {
                                            LogV3.log(t);
                                        }
                                    }
                                });
                            }
                        } catch (NumberFormatException e) {
                            LogV3.warning("AdminRunner: PROCESS_STARTED invalid pid: " + pidStr);
                        }
                    }
                    return;
                }
                if (AdminHelperProcess.CMD_RESULT.equals(response.getType())) {
                    try {
                        result.set(parseCmdResult(response.getMessage()));
                        if (AdminExecuter.isVerbose()) {
                            LogV3.info("AdminRunner [verbose]: parseCmdResult ok, latch.countDown");
                        }
                    } catch (Throwable t) {
                        error.set(t instanceof Exception ? (Exception) t : new Exception(t));
                        if (AdminExecuter.isVerbose()) {
                            LogV3.warning("AdminRunner [verbose]: parseCmdResult error: " + t.getMessage());
                        }
                    }
                    latch.countDown();
                }
            }
        };
        try {
            if (AdminExecuter.isVerbose()) {
                LogV3.info("AdminRunner [verbose]: sendToRunningInstance RUN_CMD jsonLen=" + json.length() + " workDirPathLen=" + workDirPath.length());
            }
            ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
            if (options != null) {
                options.setRequestId(String.valueOf(org.appwork.utils.UniqueAlltimeID.next()));
            }
            String optionsJson = opts.toHelperPayloadJson();
            client.sendToRunningInstance(AdminExecuter.wrapResponseListener(listener), AdminHelperProcess.RUN_CMD, json, workDirPath, optionsJson);
        } catch (ExceptionInRunningInstance exRun) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: ExceptionInRunningInstance " + exRun.getMessage());
            }
            throw new Exception("Helper reported exception: " + exRun.getMessage(), exRun);
        } catch (InvalidResponseID exInv) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: InvalidResponseID " + exInv.getMessage());
            }
            throw new Exception("Invalid response from helper: " + exInv.getMessage(), exInv);
        } catch (NoPortFileException exPort) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: NoPortFileException " + exPort.getMessage());
            }
            throw new Exception("Helper not ready (no port file): " + exPort.getMessage(), exPort);
        } catch (ErrorReadingResponseException exRead) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: ErrorReadingResponseException " + exRead.getMessage());
            }
            throw new Exception("Error reading helper response: " + exRead.getMessage(), exRead);
        }
        if (AdminExecuter.isVerbose()) {
            LogV3.info("AdminRunner [verbose]: awaiting latch timeoutMs=" + RESPONSE_TIMEOUT_MS);
        }
        if (!latch.await(RESPONSE_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: latch timeout");
            }
            throw new Exception("Timeout waiting for CMD_RESULT from helper");
        }
        Exception ex = error.get();
        if (ex != null) {
            throw ex;
        }
        ProcessOutput out = result.get();
        if (out == null) {
            if (AdminExecuter.isVerbose()) {
                LogV3.warning("AdminRunner [verbose]: result is null after latch");
            }
            throw new Exception("No CMD_RESULT received from helper");
        }
        String stdoutStr = out.getStdOutString();
        String stderrStr = out.getErrOutString();
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        AdminExecuter.streamRemoteOutputToLogV3("runAsAdmin", stdoutStr, stderrStr, opts.getLogCallback());
        LogV3.info("runAsAdmin: command finished: exitCode=" + out.getExitCode() + ", stdoutLength=" + (stdoutStr != null ? stdoutStr.length() : 0) + ", stderrLength=" + (stderrStr != null ? stderrStr.length() : 0));
        if (AdminExecuter.isVerbose()) {
            LogV3.info("AdminRunner [verbose]: runCommand done exitCode=" + out.getExitCode() + " stdoutLen=" + (stdoutStr != null ? stdoutStr.length() : 0) + " stderrLen=" + (stderrStr != null ? stderrStr.length() : 0));
        }
        return out;
    }

    /**
     * Parses the CMD_RESULT message format (exitCode, stdout hex, stderr hex) into a ProcessOutput.
     */
    public static ProcessOutput parseCmdResult(String message) throws IOException {
        if (message == null) {
            message = "";
        }
        String[] parts = message.split("\n", 3);
        int exitCode = 0;
        String stdoutHex = "";
        String stderrHex = "";
        if (parts.length >= 1 && parts[0].length() > 0) {
            try {
                exitCode = Integer.parseInt(parts[0].trim(), 10);
            } catch (NumberFormatException e) {
                // ignore
            }
        }
        if (parts.length >= 2) {
            stdoutHex = parts[1];
        }
        if (parts.length >= 3) {
            stderrHex = parts[2];
        }
        if (AdminExecuter.isVerbose()) {
            LogV3.info("AdminRunner [verbose]: parseCmdResult parts=" + parts.length + " exitCode=" + exitCode + " stdoutHexLen=" + stdoutHex.length() + " stderrHexLen=" + stderrHex.length());
        }
        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        ByteArrayOutputStream stderr = new ByteArrayOutputStream();
        if (stdoutHex.length() > 0) {
            try {
                stdout.write(HexFormatter.hexToByteArray(stdoutHex));
            } catch (Throwable t) {
                stdout.write(("decode error: " + t.getMessage()).getBytes(UTF8));
            }
        }
        if (stderrHex.length() > 0) {
            try {
                stderr.write(HexFormatter.hexToByteArray(stderrHex));
            } catch (Throwable t) {
                stderr.write(("decode error: " + t.getMessage()).getBytes(UTF8));
            }
        }
        return new ProcessOutput(exitCode, stdout, stderr, "UTF-8");
    }
}
