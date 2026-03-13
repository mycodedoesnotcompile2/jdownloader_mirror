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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.RandomAccessFile;
import java.net.ConnectException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.charset.Charset;
import java.security.KeyPair;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.experimental.windowsexecuter.ExecuteOptions;
import org.appwork.experimental.windowsexecuter.WindowsExecuter;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.windows.jna.WindowsJNAProcessUtils;
import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.NotSupportedException;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;
import org.appwork.utils.singleapp.ExceptionInRunningInstance;
import org.appwork.utils.singleapp.Response;
import org.appwork.utils.singleapp.ResponseListener;
import org.appwork.utils.singleapp.SingleAppInstance;
import org.appwork.utils.singleapp.SingleAppInstance.ErrorReadingResponseException;
import org.appwork.utils.singleapp.SingleAppInstance.InvalidResponseID;
import org.appwork.utils.singleapp.SingleAppInstance.NoPortFileException;

/**
 * Executes code with elevated privileges (Windows UAC) via a separate helper process. Uses SingleAppInstance for communication: the helper
 * accepts connections only from the parent PID. Use {@link #runAsAdmin(ElevatedTestTask, TypeRef)} or
 * {@link #runAsAdmin(File, String[], ProcessOptions)}.
 */
public final class AdminExecuter {
    private static final Charset    UTF8                          = Charset.forName("UTF-8");
    private static final int        HELPER_WAIT_MS                = 30000;
    private static final int        HELPER_POLL_MS                = 200;
    /** Poll interval for helper liveness check when waiting for response (detect unexpected helper exit). */
    private static final int        HELPER_LIVENESS_POLL_MS       = 2000;
    /** Key pair for helper channel encryption; used to decrypt ENCRYPTED responses (symmetric AES decryption, key exchange via RSA). */
    private static volatile KeyPair helperKeyPair                 = null;
    /** Environment variable: lock directory of the shared helper (set by PostBuildRunner when it starts the helper). */
    public static final String      ENV_HELPER_LOCK_DIR           = "AWTEST_HELPER_LOCK_DIR";
    /** Environment variable: Base64-encoded PKCS#8 private key for decrypting helper responses (set by PostBuildRunner). */
    public static final String      ENV_HELPER_PRIVATE_KEY_BASE64 = "AWTEST_HELPER_PRIVATE_KEY_BASE64";
    /**
     * Environment variable: application root used by the helper for SingleAppInstance ID; when set in a child process, ensures client ID
     * matches helper server ID.
     */
    public static final String      ENV_HELPER_APP_ROOT           = "AWTEST_HELPER_APP_ROOT";
    /**
     * Environment variable: random channel ID for helper communication. When set (e.g. by process that started the helper), clients use
     * this as SingleAppInstance ID so parallel tests can each open their own channel without depending on app root.
     */
    public static final String      ENV_HELPER_CHANNEL_ID         = "AWTEST_HELPER_CHANNEL_ID";
    /** When this process starts the helper, we store the generated channel ID here and in getHelperConnectionEnv() for child processes. */
    private static volatile String  helperChannelId               = null;

    /**
     * Returns true if the client socket's remote PID is allowed. Used by {@link AdminHelperSingleAppInstance} to restrict the helper to
     * connections from: (a) ancestors of the helper process, or (b) descendants of the helper's direct parent (e.g. PostBuildRunner and the
     * PostBuild tests it starts). allowedPids is the helper's ancestor chain [helper.parent, helper.grandparent, ...]. RemotePIDA is
     * allowed iff it is in allowedPids (ancestor of helper) or the helper's direct parent (allowedPids.get(0)) is an ancestor of RemotePIDA
     * (RemotePIDA is descendant of helper.parent). Resolves PID via getDesktopSupport().getPIDForRemoteAddress; returns false if PID cannot
     * be resolved or is not allowed.
     *
     * @param client
     *            the client socket
     * @param allowedPids
     *            helper's ancestor chain; first element is helper's direct parent (null or empty = accept all)
     * @return true if the connection is allowed
     */
    public static boolean isIncomingSocketAllowed(Socket client, List<Integer> allowedPids) {
        if (allowedPids == null || allowedPids.isEmpty()) {
            return true;
        }
        java.net.SocketAddress remote = client != null ? client.getRemoteSocketAddress() : null;
        if (remote == null) {
            return false;
        }
        try {
            int clientPid = WindowsJNAProcessUtils.getPIDForRemoteAddress(remote);
            if (clientPid < 0) {
                LogV3.info("AdminExecuter: rejecting connection - could not resolve remote PID for " + remote);
                return false;
            }
            // (a) RemotePIDA is an ancestor of the helper (in helper's ancestor list)
            boolean allowed = allowedPids.contains(Integer.valueOf(clientPid));
            if (!allowed) {
                // (b) RemotePIDA is a descendant of the helper's direct parent (e.g. PostBuild test started by PostBuildRunner)
                allowed = hasHelperDirectParentAsAncestor(clientPid, allowedPids);
            }
            if (!allowed) {
                LogV3.info("AdminExecuter: rejecting connection - remote PID " + clientPid + " not an ancestor of helper and not a descendant of helper.parent");
            }
            return allowed;
        } catch (Throwable e) {
            LogV3.log(e);
            return false;
        }
    }

    /**
     * Returns true if the helper's direct parent (first element of allowedPids) is an ancestor of the given pid. So the given pid is the
     * helper's parent or a descendant of it (e.g. PostBuild test process). We do not allow descendants of helper.grandparent that are not
     * also descendants of helper.parent.
     *
     * @param pid
     *            remote process ID (e.g. PostBuild test)
     * @param allowedPids
     *            helper's ancestor chain; allowedPids.get(0) is the helper's direct parent
     * @return true if walking up from pid we find the helper's direct parent
     */
    private static boolean hasHelperDirectParentAsAncestor(int pid, List<Integer> allowedPids) {
        if (allowedPids == null || allowedPids.isEmpty()) {
            return false;
        }
        int helperDirectParentPid = allowedPids.get(0).intValue();
        try {
            org.appwork.processes.ProcessHandler handler = ProcessHandlerFactory.getProcessHandler();
            int current = pid;
            int maxSteps = 25;
            while (current > 0 && maxSteps-- > 0) {
                List<ProcessInfo> list = handler.listByPids(current);
                if (list == null || list.isEmpty()) {
                    break;
                }
                int parent = list.get(0).getParentPid();
                if (parent <= 0 || parent == current) {
                    break;
                }
                if (parent == helperDirectParentPid) {
                    LogV3.info("AdminExecuter: allowing connection - remote PID " + pid + " has helper's direct parent PID " + helperDirectParentPid + " as ancestor");
                    return true;
                }
                current = parent;
            }
        } catch (Throwable e) {
            LogV3.info("AdminExecuter: hasHelperDirectParentAsAncestor failed for pid " + pid + ": " + e.getMessage());
        }
        return false;
    }

    /**
     * System property to enable debug logging for the elevated helper (writes stdout/stderr to files). Set to "true" to enable.
     */
    public static final String RUN_AS_ADMIN_DEBUG_HELPER_LOGS = "awtest.runAsAdmin.debugHelperLogs";
    /**
     * System property to show the AdminHelperProcess debug window (parent-gone and shutdown logs). Set to "true" to enable.
     */
    public static final String RUN_AS_ADMIN_SHOW_DEBUG_WINDOW = "awtest.runAsAdmin.showDebugWindow";
    /**
     * System property for intensive verbose logging (client and helper) to find errors. Set to "true" to enable.
     */
    public static final String RUN_AS_ADMIN_VERBOSE           = "awtest.runAsAdmin.verbose";

    static boolean isVerbose() {
        return "true".equalsIgnoreCase(System.getProperty(RUN_AS_ADMIN_VERBOSE));
    }

    /**
     * Wraps a ResponseListener to decrypt ENCRYPTED responses from the helper (symmetric AES; key exchange via RSA). If no key pair is set
     * or the response type is not ENCRYPTED, forwards to the inner listener as-is.
     */
    public static ResponseListener wrapResponseListener(final ResponseListener inner) {
        return new ResponseListener() {
            @Override
            public void onConnected(SingleAppInstance single, java.net.SocketAddress remoteSocket, String[] message) {
                inner.onConnected(single, remoteSocket, message);
            }

            @Override
            public void onReceivedResponse(SingleAppInstance single, Response response) {
                if (AdminHelperProcess.ENCRYPTED.equals(response.getType()) && helperKeyPair != null) {
                    try {
                        String msg = response.getMessage();
                        if (msg != null && msg.length() > 0) {
                            byte[] encrypted = org.appwork.utils.encoding.Base64.decode(msg);
                            if (encrypted != null && encrypted.length > 0) {
                                byte[] decrypted = HelperChannelCrypto.decryptWithPrivateKey(helperKeyPair.getPrivate(), encrypted);
                                String dec = new String(decrypted, UTF8);
                                int idx = dec.indexOf('\n');
                                String type = idx >= 0 ? dec.substring(0, idx) : dec;
                                String message = idx >= 0 && idx + 1 <= dec.length() ? dec.substring(idx + 1) : "";
                                response = new Response(type, message);
                            }
                        }
                    } catch (Throwable t) {
                        inner.onReceivedResponse(single, new Response(SingleAppInstance.EXCEPTION, "Decrypt failed: " + t.getMessage()));
                        return;
                    }
                }
                inner.onReceivedResponse(single, response);
            }
        };
    }

    /**
     * Runs the given task in the elevated helper process (one UAC prompt). The task is serialized, sent to the helper, executed there with
     * an {@link AdminRunnerLocal}, and the serializable return value is sent back.
     *
     * @param task
     *            serializable task to run in the elevated process
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT}); used to cast the result to T
     * @return the result returned by {@link ElevatedTestTask#run()} (may be null), cast to T
     */
    @SuppressWarnings("unchecked")
    public static <T> T runAsAdmin(ElevatedTestTask task, TypeRef<T> resultType) throws Exception {
        return runAsAdmin(task, resultType, ProcessOptions.DEFAULT);
    }

    /**
     * Runs the given task in the elevated helper (one UAC prompt). The task is run in a separate JRE process; the helper sends
     * PROCESS_STARTED(pid) when that process is started. options.setTaskPID(pid) and options.setTerminateAction(...) are set so
     * {@link ProcessOptions#terminate()} terminates the remote process (TERMINATE_REQUEST_ID). If options.keepRunning is true, the remote
     * process is not terminated when the task completes; the return value is still returned. returns immediately after starting the process
     * without waiting for the task result.
     *
     * @param task
     *            serializable task to run
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT})
     * @param options
     *            process options (waitFor, keepRunning, cancelCallback); null uses {@link ProcessOptions#DEFAULT}
     * @return the result returned by {@link ElevatedTestTask#run()} (may be null), cast to T
     */
    @SuppressWarnings("unchecked")
    public static <T> T runAsAdmin(ElevatedTestTask task, TypeRef<T> resultType, ProcessOptions options) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("runAsAdmin is only supported on Windows");
        }
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsAdmin(ElevatedTestTask, ProcessOptions) entered");
        }
        // When already elevated, run the task in-process instead of delegating to the helper.
        if (WindowsUtils.isElevated()) {
            if (isVerbose()) {
                LogV3.info("AdminExecuter [verbose]: already elevated, running task in-process");
            }
            Object raw = task.run();
            if (raw == null) {
                return null;
            }
            if (resultType != null && resultType.getRawClass() != null) {
                return (T) resultType.getRawClass().cast(raw);
            }
            return (T) raw;
        }
        ensureHelperRunning();
        byte[] taskBytes = serializeTask(task);
        String hexTask = HexFormatter.byteArrayToHex(taskBytes);
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: task serialized, len=" + taskBytes.length + " hexLen=" + hexTask.length());
        }
        opts.setRequestId(String.valueOf(UniqueAlltimeID.next()));
        String optionsJson = opts.toHelperPayloadJson();
        final AtomicReference<SingleAppInstance> clientRef = new AtomicReference<SingleAppInstance>(null);
        final CountDownLatch latch = new CountDownLatch(1);
        final AtomicReference<Object> resultRef = new AtomicReference<Object>(null);
        final AtomicReference<Exception> errorRef = new AtomicReference<Exception>(null);
        final ProcessOptions optsRef = opts;
        ResponseListener listener = new ResponseListener() {
            @Override
            public void onConnected(SingleAppInstance single, java.net.SocketAddress remoteSocket, String[] message) {
            }

            @Override
            public void onReceivedResponse(SingleAppInstance single, Response response) {
                if (AdminHelperProcess.PROCESS_STARTED.equals(response.getType())) {
                    String pidStr = response.getMessage();
                    if (pidStr != null && pidStr.trim().length() > 0 && optsRef != null) {
                        try {
                            int pid = Integer.parseInt(pidStr.trim(), 10);
                            optsRef.setTaskPID(Integer.valueOf(pid));
                            final String reqId = optsRef.getRequestId();
                            if (reqId != null) {
                                final AtomicReference<SingleAppInstance> ref = clientRef;
                                optsRef.setTerminateAction(new Runnable() {
                                    @Override
                                    public void run() {
                                        try {
                                            SingleAppInstance c = ref.get();
                                            if (c != null) {
                                                c.sendToRunningInstance(null, AdminHelperProcess.TERMINATE_REQUEST_ID, reqId);
                                            }
                                        } catch (Throwable t) {
                                            LogV3.log(t);
                                        }
                                    }
                                });
                            }
                        } catch (NumberFormatException e) {
                            LogV3.warning("AdminExecuter: runAsAdmin PROCESS_STARTED invalid pid: " + pidStr);
                        }
                    }
                    return;
                }
                if (AdminHelperProcess.TASK_RESULT.equals(response.getType())) {
                    try {
                        String hexResult = response.getMessage();
                        resultRef.set(deserializeTaskResult(hexResult != null ? hexResult : "", optsRef != null ? optsRef.getLogCallback() : null));
                    } catch (Throwable t) {
                        errorRef.set(t instanceof Exception ? (Exception) t : new Exception(t));
                    }
                    latch.countDown();
                } else if (SingleAppInstance.EXCEPTION.equals(response.getType())) {
                    errorRef.set(new Exception("Helper reported: " + response.getMessage()));
                    latch.countDown();
                }
            }
        };
        for (int attempt = 0; attempt < 2; attempt++) {
            if (attempt > 0) {
                LogV3.warning("AdminExecuter: connection refused, retrying after ensureHelperRunning");
                ensureHelperRunning();
            }
            SingleAppInstance client = createHelperClient();
            clientRef.set(client);
            try {
                client.sendToRunningInstance(wrapResponseListener(listener), AdminHelperProcess.RUN_TASK, hexTask, optionsJson);
                break;
            } catch (ConnectException e) {
                if (attempt == 0) {
                    continue;
                }
                throw new Exception("Helper not reachable after retry: " + e.getMessage(), e);
            } catch (ExceptionInRunningInstance e) {
                throw new Exception("Helper reported exception: " + e.getMessage(), e);
            } catch (InvalidResponseID e) {
                throw new Exception("Invalid response from helper: " + e.getMessage(), e);
            } catch (NoPortFileException e) {
                throw new Exception("Helper not ready (no port file): " + e.getMessage(), e);
            } catch (ErrorReadingResponseException e) {
                throw new Exception("Error reading helper response: " + e.getMessage(), e);
            }
        }
        startHelperLivenessWatcher(getHelperLockFile(), latch, errorRef);
        if (!latch.await(120000, TimeUnit.MILLISECONDS)) {
            throw new Exception("Timeout waiting for TASK_RESULT from helper");
        }
        Exception ex = errorRef.get();
        if (ex != null) {
            throw ex;
        }
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsAdmin(ElevatedTestTask, ProcessOptions) finished");
        }
        Object raw = resultRef.get();
        if (raw == null) {
            return null;
        }
        if (resultType != null && resultType.getRawClass() != null) {
            return (T) resultType.getRawClass().cast(raw);
        }
        return (T) raw;
    }

    /**
     * Runs the given task as NT AUTHORITY\SYSTEM (LocalSystem). Ensures an elevated helper is running, then runs the task in a scheduled
     * task as SYSTEM (via {@link RunTaskAsSystemMain}). Stdout/stderr from the task are forwarded to the calling process.
     *
     * @param task
     *            serializable task to run as Local System
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT}); used to cast the result to T
     * @return the result returned by {@link ElevatedTestTask#run()} (may be null), cast to T
     */
    @SuppressWarnings("unchecked")
    public static <T> T runAsLocalSystem(ElevatedTestTask task, TypeRef<T> resultType, ProcessOptions options) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("runAsLocalSystem is only supported on Windows");
        }
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsLocalSystem(ElevatedTestTask) entered");
        }
        // When already running as LocalSystem, run the task in-process instead of delegating to the helper (avoids nested schtasks etc.).
        if (WindowsUtils.isRunningAsLocalSystem()) {
            if (isVerbose()) {
                LogV3.info("AdminExecuter [verbose]: already LocalSystem, running task in-process");
            }
            Object raw = task.run();
            if (raw == null) {
                return null;
            }
            if (resultType != null && resultType.getRawClass() != null) {
                return (T) resultType.getRawClass().cast(raw);
            }
            return (T) raw;
        }
        ensureHelperRunning();
        byte[] taskBytes = serializeTask(task);
        String hexTask = HexFormatter.byteArrayToHex(taskBytes);
        String classpath = getAbsoluteClassPathForLocalSystem();
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsLocalSystem(ElevatedTestTask) sending RUN_AS_LOCAL_SYSTEM_TASK to helper");
        }
        SingleAppInstance client = createHelperClient();
        final CountDownLatch latch = new CountDownLatch(1);
        final AtomicReference<String> hexResultRef = new AtomicReference<String>(null);
        final AtomicReference<Exception> errorRef = new AtomicReference<Exception>(null);
        ResponseListener listener = new ResponseListener() {
            @Override
            public void onReceivedResponse(SingleAppInstance single, Response response) {
                if (AdminHelperProcess.TASK_RESULT.equals(response.getType())) {
                    hexResultRef.set(response.getMessage() != null ? response.getMessage() : "");
                    latch.countDown();
                } else if (SingleAppInstance.EXCEPTION.equals(response.getType())) {
                    errorRef.set(new Exception("Helper reported: " + response.getMessage()));
                    latch.countDown();
                }
            }

            @Override
            public void onConnected(SingleAppInstance instance, SocketAddress remoteSocket, String[] message) {
            }
        };
        for (int attempt = 0; attempt < 2; attempt++) {
            if (attempt > 0) {
                LogV3.warning("AdminExecuter: connection refused (runAsLocalSystem task), retrying after ensureHelperRunning");
                ensureHelperRunning();
                client = createHelperClient();
            }
            try {
                client.sendToRunningInstance(wrapResponseListener(listener), AdminHelperProcess.RUN_AS_LOCAL_SYSTEM_TASK, hexTask, classpath);
                break;
            } catch (ConnectException e) {
                if (attempt == 0) {
                    continue;
                }
                throw new Exception("Helper not reachable after retry: " + e.getMessage(), e);
            } catch (ExceptionInRunningInstance e) {
                throw new Exception("Helper reported exception: " + e.getMessage(), e);
            } catch (InvalidResponseID e) {
                throw new Exception("Invalid response from helper: " + e.getMessage(), e);
            } catch (NoPortFileException e) {
                throw new Exception("Helper not ready (no port file): " + e.getMessage(), e);
            } catch (ErrorReadingResponseException e) {
                throw new Exception("Error reading helper response: " + e.getMessage(), e);
            }
        }
        startHelperLivenessWatcher(getHelperLockFile(), latch, errorRef);
        if (!latch.await(300000, TimeUnit.MILLISECONDS)) {
            throw new Exception("Timeout waiting for TASK_RESULT from helper (runAsLocalSystem)");
        }
        Exception ex = errorRef.get();
        if (ex != null) {
            throw ex;
        }
        String hex = hexResultRef.get();
        if (hex == null || hex.trim().length() == 0) {
            throw new Exception("runAsLocalSystem task produced no result (empty response)");
        }
        Object result = deserializeTaskResult(hex.trim());
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsLocalSystem(ElevatedTestTask) finished");
        }
        Object raw = result;
        if (raw == null) {
            return null;
        }
        if (resultType != null && resultType.getRawClass() != null) {
            return (T) resultType.getRawClass().cast(raw);
        }
        return (T) raw;
    }

    /**
     * Returns the current JVM classpath with all entries converted to absolute paths. Used when sending the classpath to the helper for
     * RUN_AS_LOCAL_SYSTEM_TASK so that the subprocess (running with CWD = temp dir) can find RunTaskAsSystemMain and dependencies
     * regardless of working directory.
     */
    private static String getAbsoluteClassPathForLocalSystem() {
        String cp = System.getProperty("java.class.path");
        if (cp == null || cp.trim().length() == 0) {
            return cp != null ? cp : "";
        }
        String sep = File.pathSeparator;
        String[] entries = cp.split(sep.equals(";") ? ";" : sep);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < entries.length; i++) {
            String e = entries[i].trim();
            if (e.length() == 0) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(sep);
            }
            sb.append(new File(e).getAbsolutePath());
        }
        return sb.toString();
    }

    private static byte[] serializeTask(ElevatedTestTask task) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = null;
        try {
            oos = new ObjectOutputStream(baos);
            oos.writeObject(task);
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

    private static Object deserializeTaskResult(String hexResult) throws IOException, ClassNotFoundException {
        return deserializeTaskResult(hexResult, null);
    }

    private static Object deserializeTaskResult(String hexResult, LogCallback logCallback) throws IOException, ClassNotFoundException {
        if (hexResult == null || hexResult.length() == 0) {
            return null;
        }
        final String cleanedHex = stripWhitespace(hexResult);
        if (cleanedHex.length() == 0) {
            throw new java.io.StreamCorruptedException("Invalid task result: empty/non-hex output. RawPrefix=" + toDebugPrefix(hexResult));
        }
        final int firstNonHex = firstNonHexIndex(cleanedHex);
        if (firstNonHex >= 0) {
            final char c = cleanedHex.charAt(firstNonHex);
            throw new java.io.StreamCorruptedException("Invalid task result: non-hex char at index " + firstNonHex + " (0x" + Integer.toHexString(c) + "). RawPrefix=" + toDebugPrefix(hexResult));
        }
        if ((cleanedHex.length() & 1) == 1) {
            throw new java.io.StreamCorruptedException("Invalid task result: odd hex length=" + cleanedHex.length() + ". RawPrefix=" + toDebugPrefix(hexResult));
        }
        byte[] bytes = HexFormatter.hexToByteArray(cleanedHex);
        if (bytes == null || bytes.length == 0) {
            return null;
        }
        ObjectInputStream ois = null;
        try {
            ois = new ObjectInputStream(new ByteArrayInputStream(bytes));
            Object obj = ois.readObject();
            if (obj instanceof AdminTaskResultWrapper) {
                AdminTaskResultWrapper wrapper = (AdminTaskResultWrapper) obj;
                if (logCallback != null) {
                    streamRemoteOutputToLogV3("runAsAdmin", wrapper.getStdout(), wrapper.getStderr(), logCallback);
                } else {
                    forwardTaskOutput(wrapper.getStdout(), wrapper.getStderr());
                }
                return wrapper.getReturnValue();
            }
            return obj;
        } catch (java.io.StreamCorruptedException e) {
            // Most common cause: non-hex text mixed into the supposed hex result (e.g. JVM messages, command wrapper output).
            // Provide the raw prefix to make debugging easy.
            throw new java.io.StreamCorruptedException(e.getMessage() + " RawPrefix=" + toDebugPrefix(hexResult));
        } finally {
            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Removes whitespace characters from the input. Any remaining non-hex characters will be rejected.
     */
    private static String stripWhitespace(String input) {
        if (input == null || input.length() == 0) {
            return "";
        }
        StringBuilder sb = new StringBuilder(input.length());
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                continue;
            }
            sb.append(c);
        }
        return sb.toString();
    }

    private static int firstNonHexIndex(String s) {
        if (s == null || s.length() == 0) {
            return -1;
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            boolean ok = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
            if (!ok) {
                return i;
            }
        }
        return -1;
    }

    private static String toDebugPrefix(String raw) {
        if (raw == null) {
            return "null";
        }
        final int max = 160;
        String s = raw;
        if (s.length() > max) {
            s = s.substring(0, max) + "...";
        }
        // Keep it single-line for logs.
        s = s.replace("\r", "\\r").replace("\n", "\\n");
        return "\"" + s + "\"";
    }

    /**
     * Forwards stdout/stderr from the elevated task to the calling process's System.out and System.err.
     */
    private static void forwardTaskOutput(String stdout, String stderr) {
        if (stdout != null && stdout.length() > 0) {
            System.out.print(stdout);
            if (!stdout.endsWith("\n")) {
                System.out.println();
            }
            System.out.flush();
        }
        if (stderr != null && stderr.length() > 0) {
            System.err.print(stderr);
            if (!stderr.endsWith("\n")) {
                System.err.println();
            }
            System.err.flush();
        }
    }

    /**
     * Streams stdout/stderr of a remote process to LogV3 (e.g. from runAsAdmin or runAsLocalSystem). Each non-empty line is logged with the
     * given prefix. If logCallback is non-null, each line is also passed to {@link LogCallback#onStdOut(String)} /
     * {@link LogCallback#onStdErr(String)}.
     */
    public static void streamRemoteOutputToLogV3(String prefix, String stdout, String stderr, LogCallback logCallback) {
        if (stdout != null && stdout.length() > 0) {
            for (String line : stdout.split("\r?\n")) {
                String trimmed = line != null ? line.trim() : "";
                if (trimmed.length() > 0) {
                    LogV3.info(prefix + " [stdout]: " + line);
                    if (logCallback != null) {
                        try {
                            logCallback.onStdOut(line);
                        } catch (Throwable t) {
                            LogV3.log(t);
                        }
                    }
                }
            }
        }
        if (stderr != null && stderr.length() > 0) {
            for (String line : stderr.split("\r?\n")) {
                String trimmed = line != null ? line.trim() : "";
                if (trimmed.length() > 0) {
                    LogV3.warning(prefix + " [stderr]: " + line);
                    if (logCallback != null) {
                        try {
                            logCallback.onStdErr(line);
                        } catch (Throwable t) {
                            LogV3.log(t);
                        }
                    }
                }
            }
        }
    }

    /**
     * Reads new bytes from file from the given position and appends decoded UTF-8 to the buffer. Returns the new file position (bytes read
     * up to). If file does not exist or length <= position, returns position unchanged.
     */
    private static long readFileTail(File file, long position, StringBuilder buffer) {
        if (file == null || !file.isFile()) {
            return position;
        }
        long length = file.length();
        if (length <= position) {
            return position;
        }
        int toRead = (int) Math.min(length - position, Integer.MAX_VALUE);
        if (toRead <= 0) {
            return position;
        }
        RandomAccessFile raf = null;
        try {
            raf = new RandomAccessFile(file, "r");
            raf.seek(position);
            byte[] b = new byte[toRead];
            int n = raf.read(b);
            if (n > 0) {
                buffer.append(new String(b, 0, n, UTF8));
                return position + n;
            }
        } catch (IOException e) {
            // File may be locked or truncated; ignore and retry next poll
        } finally {
            if (raf != null) {
                try {
                    raf.close();
                } catch (IOException e) {
                }
            }
        }
        return position;
    }

    /**
     * Extracts complete lines (terminated by \n) from the buffer, forwards each to LogV3 and optional callback, and removes them from the
     * buffer.
     */
    private static void streamLinesFromBuffer(StringBuilder buffer, String prefix, boolean isStderr, LogCallback logCallback) {
        while (buffer.length() > 0) {
            int idx = -1;
            for (int i = 0; i < buffer.length(); i++) {
                if (buffer.charAt(i) == '\n') {
                    idx = i;
                    break;
                }
            }
            if (idx < 0) {
                break;
            }
            String line = buffer.substring(0, idx);
            if (line.endsWith("\r")) {
                line = line.substring(0, line.length() - 1);
            }
            buffer.delete(0, idx + 1);
            String trimmed = line != null ? line.trim() : "";
            if (trimmed.length() > 0) {
                if (isStderr) {
                    LogV3.warning(prefix + " [stderr]: " + line);
                    if (logCallback != null) {
                        try {
                            logCallback.onStdErr(line);
                        } catch (Throwable t) {
                            LogV3.log(t);
                        }
                    }
                } else {
                    LogV3.info(prefix + " [stdout]: " + line);
                    if (logCallback != null) {
                        try {
                            logCallback.onStdOut(line);
                        } catch (Throwable t) {
                            LogV3.log(t);
                        }
                    }
                }
            }
        }
    }

    /**
     * Runs a single command with elevated privileges (one UAC prompt if helper not already running). Returns the process output.
     *
     * @param workDir
     *            working directory (null allowed)
     * @param cmd
     *            command and arguments
     * @param options
     *            launch options (waitFor, env); null uses {@link ProcessOptions#DEFAULT}
     */
    public static ProcessOutput runAsAdmin(File workDir, String[] cmd, ProcessOptions options) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("runAsAdmin is only supported on Windows");
        }
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsAdmin(File, String[]) entered workDir=" + workDir + " cmd=" + java.util.Arrays.toString(cmd) + " waitFor=" + opts.isWaitFor());
        }
        // When already elevated, run the command in-process instead of delegating to the helper.
        if (WindowsUtils.isElevated()) {
            if (isVerbose()) {
                LogV3.info("AdminExecuter [verbose]: already elevated, running command in-process");
            }
            ProcessBuilder pb = ProcessBuilderFactory.create(cmd != null ? cmd : new String[0]);
            if (workDir != null && workDir.isDirectory()) {
                pb.directory(workDir);
            }
            return ProcessBuilderFactory.runCommand(pb);
        }
        ensureHelperRunning();
        for (int attempt = 0; attempt < 2; attempt++) {
            if (attempt > 0) {
                LogV3.warning("AdminExecuter: connection refused (runAsAdmin cmd), retrying after ensureHelperRunning");
                ensureHelperRunning();
            }
            SingleAppInstance client = createHelperClient();
            AdminRunner runner = new AdminRunner(client);
            try {
                ProcessOutput out = runner.runCommand(workDir, java.util.Arrays.asList(cmd), opts);
                if (isVerbose()) {
                    LogV3.info("AdminExecuter [verbose]: runAsAdmin(File, String[]) finished exitCode=" + (out != null ? out.getExitCode() : -1));
                }
                return out;
            } catch (Exception e) {
                Throwable cause = e;
                while (cause != null && !(cause instanceof ConnectException)) {
                    cause = cause.getCause();
                }
                if (cause instanceof ConnectException && attempt == 0) {
                    continue;
                }
                if (cause instanceof ConnectException && attempt > 0) {
                    throw new Exception("Helper not reachable after retry: " + e.getMessage(), e);
                }
                throw e;
            }
        }
        throw new Exception("Helper not reachable");
    }

    /**
     * Runs a static method with elevated privileges (one UAC prompt). Parameter types are inferred from the argument types (boxed types map
     * to primitives, e.g. Integer -> int). For null arguments, Object.class is used. Parameters and return value must be serializable (or
     * null).
     *
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT})
     */
    public static <T> T runAsAdmin(final Class<?> clazz, final String methodName, final TypeRef<T> resultType, final Object... args) throws Exception {
        return runAsAdmin(clazz, methodName, resultType, null, args);
    }

    /**
     * Runs a static method with elevated privileges (one UAC prompt). If the method returns a {@link ResultWithCancel}, options receive a
     * cancel action so {@link ProcessOptions#terminate()} stops/cancels the remote operation; the returned value is the unwrapped result
     * (e.g. port). Otherwise behaves like {@link #runAsAdmin(Class, String, TypeRef, Object...)}.
     *
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#INT} for port)
     * @param options
     *            optional; used for requestId and cancel action when PROCESS_STARTED is received
     */
    @SuppressWarnings("unchecked")
    public static <T> T runAsAdmin(final Class<?> clazz, final String methodName, final TypeRef<T> resultType, final ProcessOptions options, final Object... args) throws Exception {
        Class<?>[] paramTypes = inferParamTypes(args);
        Object[] a = (args != null ? args : new Object[0]);
        String[] paramTypeNames = new String[paramTypes.length];
        for (int i = 0; i < paramTypes.length; i++) {
            paramTypeNames[i] = paramTypes[i].getName();
        }
        ElevatedTestTask task = new MethodInvocationTask(clazz.getName(), methodName, paramTypeNames, a);
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        Object raw = runAsAdmin(task, TypeRef.OBJECT, opts);
        if (raw == null) {
            return null;
        }
        if (resultType != null && resultType.getRawClass() != null) {
            return (T) resultType.getRawClass().cast(raw);
        }
        return (T) raw;
    }

    /**
     * Runs a static method with elevated privileges (one UAC prompt). The method is invoked by reflection in the elevated helper.
     * Parameters and return value must be serializable (or null).
     *
     * @param clazz
     *            class that declares the static method
     * @param methodName
     *            name of the static method
     * @param paramTypes
     *            parameter types (e.g. String.class, Integer.TYPE)
     * @param args
     *            arguments (must match paramTypes)
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT})
     * @return the method's return value (serializable), or null, cast to T
     */
    public static <T> T runAsAdmin(final Class<?> clazz, final String methodName, final Class<?>[] paramTypes, final Object[] args, final TypeRef<T> resultType) throws Exception {
        String[] paramTypeNames = new String[paramTypes != null ? paramTypes.length : 0];
        if (paramTypes != null) {
            for (int i = 0; i < paramTypes.length; i++) {
                paramTypeNames[i] = paramTypes[i].getName();
            }
        }
        Object[] a = (args != null ? args : new Object[0]);
        return runAsAdmin(new MethodInvocationTask(clazz.getName(), methodName, paramTypeNames, a), resultType);
    }

    private static Class<?>[] inferParamTypes(Object[] args) {
        if (args == null || args.length == 0) {
            return new Class<?>[0];
        }
        Class<?>[] paramTypes = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            if (args[i] == null) {
                paramTypes[i] = Object.class;
            } else {
                paramTypes[i] = toPrimitiveIfBoxed(args[i].getClass());
            }
        }
        return paramTypes;
    }

    private static Class<?> toPrimitiveIfBoxed(Class<?> c) {
        if (c == Integer.class) {
            return Integer.TYPE;
        }
        if (c == Long.class) {
            return Long.TYPE;
        }
        if (c == Boolean.class) {
            return Boolean.TYPE;
        }
        if (c == Byte.class) {
            return Byte.TYPE;
        }
        if (c == Short.class) {
            return Short.TYPE;
        }
        if (c == Character.class) {
            return Character.TYPE;
        }
        if (c == Double.class) {
            return Double.TYPE;
        }
        if (c == Float.class) {
            return Float.TYPE;
        }
        return c;
    }

    /**
     * Runs a single command as NT AUTHORITY\SYSTEM (LocalSystem). Requires admin rights: first ensures an elevated helper is running (same
     * as {@link #runAsAdmin}, one UAC prompt if needed), then runs the command as LocalSystem via schtasks (or JNA when available).
     *
     * @param workDir
     *            working directory (null allowed)
     * @param cmd
     *            command and arguments
     * @param options
     *            launch options (waitFor, env); null uses {@link ProcessOptions#DEFAULT}
     */
    public static ProcessOutput runAsLocalSystem(File workDir, String[] cmd, ProcessOptions options) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("runAsLocalSystem is only supported on Windows");
        }
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsLocalSystem entered workDir=" + workDir + " cmd=" + java.util.Arrays.toString(cmd) + " waitFor=" + opts.isWaitFor());
        }
        // When already LocalSystem, run the command in-process instead of delegating to the helper.
        if (WindowsUtils.isRunningAsLocalSystem()) {
            if (isVerbose()) {
                LogV3.info("AdminExecuter [verbose]: already LocalSystem, running command in-process");
            }
            ProcessBuilder pb = ProcessBuilderFactory.create(cmd != null ? cmd : new String[0]);
            if (workDir != null && workDir.isDirectory()) {
                pb.directory(workDir);
            }
            return ProcessBuilderFactory.runCommand(pb);
        }
        // Run as LocalSystem only works with admin; ensure elevated helper first (runAsAdmin / UAC if needed).
        ensureHelperRunning();
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: ensureHelperRunning done, sending RUN_AS_LOCAL_SYSTEM_CMD to helper");
        }
        return runAsLocalSystemViaHelper(workDir, cmd, options);
    }

    /**
     * Runs a command as non-elevated user. Use this when already inside a runAsAdmin or runAsLocalSystem task and something must run with
     * normal user rights (e.g. start UI, access HKCU, or avoid admin-only APIs). When the current process is already non-elevated, the
     * command runs in-process. When elevated or LocalSystem, delegates to {@link WindowsExecuter#runAsNonElevatedUser(ExecuteOptions)}.
     *
     * @param options
     *            command, workingDir, waitFor, optional logCallback; must not be null
     * @return process output; when waitFor is false, exitCode is -1 and remotePid may be set
     */
    public static ProcessOutput runAsNonElevatedUser(ExecuteOptions options) throws Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("runAsNonElevatedUser is only supported on Windows");
        }
        if (options == null) {
            throw new IllegalArgumentException("options cannot be null");
        }
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsNonElevatedUser entered cmd=" + java.util.Arrays.toString(options.getCmd()) + " waitFor=" + options.isWaitFor());
        }
        if (WindowsUtils.isElevated() || WindowsUtils.isRunningAsLocalSystem()) {
            if (isVerbose()) {
                LogV3.info("AdminExecuter [verbose]: elevated or LocalSystem, delegating to WindowsExecuter.runAsNonElevatedUser");
            }
            return WindowsExecuter.runAsNonElevatedUser(options);
        }
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: already non-elevated, running command in-process");
        }
        String[] cmd = options.getCmd();
        ProcessBuilder pb = ProcessBuilderFactory.create(cmd != null ? cmd : new String[0]);
        File workDir = options.getWorkingDir();
        if (workDir != null && workDir.isDirectory()) {
            pb.directory(workDir);
        }
        if (options.isWaitFor()) {
            return ProcessBuilderFactory.runCommand(pb);
        }
        pb.start();
        String codePage = "UTF-8";
        try {
            codePage = ProcessBuilderFactory.getConsoleCodepage();
        } catch (Throwable t) {
            // ignore
        }
        return new ProcessOutput(-1, new ByteArrayOutputStream(), new ByteArrayOutputStream(), codePage, null, null);
    }

    /**
     * Runs a single command as non-elevated user. Convenience overload that builds {@link ExecuteOptions} from workDir, cmd and options.
     *
     * @param workDir
     *            working directory (null allowed)
     * @param cmd
     *            command and arguments
     * @param options
     *            launch options (waitFor, logCallback); null uses {@link ProcessOptions#DEFAULT}
     */
    public static ProcessOutput runAsNonElevatedUser(File workDir, String[] cmd, ProcessOptions options) throws Exception {
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        ExecuteOptions.Builder b = ExecuteOptions.builder().cmd(cmd != null ? cmd : new String[0]).waitFor(opts.isWaitFor());
        if (workDir != null && workDir.isDirectory()) {
            b.workingDir(workDir);
        }
        if (opts.getLogCallback() != null) {
            b.logCallback(opts.getLogCallback());
        }
        return runAsNonElevatedUser(b.build());
    }

    /**
     * Sends RUN_AS_LOCAL_SYSTEM_CMD to the helper and waits for CMD_RESULT. Helper runs the command as Local System and forwards logs. When
     * the helper sends PROCESS_STARTED, options.setTaskPID(pid) and options.setTerminateAction(...) are set so
     * {@link ProcessOptions#terminate()} terminates the remote process via TERMINATE_REQUEST_ID.
     */
    private static ProcessOutput runAsLocalSystemViaHelper(File workDir, String[] cmd, ProcessOptions options) throws Exception {
        ProcessOptions opts = options != null ? options : ProcessOptions.DEFAULT;
        if (options != null) {
            options.setRequestId(String.valueOf(UniqueAlltimeID.next()));
        }
        String workDirPath = (workDir != null && workDir.isDirectory()) ? workDir.getAbsolutePath() : "";
        String jsonCmd = Deser.toString(Arrays.asList(cmd));
        String optionsJson = opts.toHelperPayloadJson();
        final AtomicReference<SingleAppInstance> clientRef = new AtomicReference<SingleAppInstance>(null);
        final CountDownLatch latch = new CountDownLatch(1);
        final AtomicReference<ProcessOutput> resultRef = new AtomicReference<ProcessOutput>(null);
        final AtomicReference<Exception> errorRef = new AtomicReference<Exception>(null);
        final ProcessOptions optsRef = opts;
        ResponseListener listener = new ResponseListener() {
            @Override
            public void onReceivedResponse(SingleAppInstance single, Response response) {
                if (AdminHelperProcess.PROCESS_STARTED.equals(response.getType())) {
                    String pidStr = response.getMessage();
                    if (pidStr != null && pidStr.trim().length() > 0 && optsRef != null) {
                        try {
                            int pid = Integer.parseInt(pidStr.trim(), 10);
                            optsRef.setTaskPID(Integer.valueOf(pid));
                            final String reqId = optsRef.getRequestId();
                            if (reqId != null) {
                                final AtomicReference<SingleAppInstance> ref = clientRef;
                                optsRef.setTerminateAction(new Runnable() {
                                    @Override
                                    public void run() {
                                        try {
                                            SingleAppInstance c = ref.get();
                                            if (c != null) {
                                                c.sendToRunningInstance(null, AdminHelperProcess.TERMINATE_REQUEST_ID, reqId);
                                            }
                                        } catch (Throwable t) {
                                            LogV3.log(t);
                                        }
                                    }
                                });
                            }
                        } catch (NumberFormatException e) {
                            LogV3.warning("AdminExecuter: runAsLocalSystem PROCESS_STARTED invalid pid: " + pidStr);
                        }
                    }
                    return;
                }
                if (AdminHelperProcess.CMD_RESULT.equals(response.getType())) {
                    try {
                        resultRef.set(AdminRunner.parseCmdResult(response.getMessage()));
                    } catch (Throwable t) {
                        errorRef.set(t instanceof Exception ? (Exception) t : new Exception(t));
                    }
                    latch.countDown();
                } else if (SingleAppInstance.EXCEPTION.equals(response.getType())) {
                    errorRef.set(new Exception("Helper reported: " + response.getMessage()));
                    latch.countDown();
                }
            }

            @Override
            public void onConnected(SingleAppInstance instance, SocketAddress remoteSocket, String[] message) {
            }
        };
        for (int attempt = 0; attempt < 2; attempt++) {
            if (attempt > 0) {
                LogV3.warning("AdminExecuter: connection refused, retrying after ensureHelperRunning");
                ensureHelperRunning();
            }
            SingleAppInstance client = createHelperClient();
            clientRef.set(client);
            try {
                client.sendToRunningInstance(wrapResponseListener(listener), AdminHelperProcess.RUN_AS_LOCAL_SYSTEM_CMD, workDirPath, jsonCmd, optionsJson);
                break;
            } catch (ConnectException e) {
                if (attempt == 0) {
                    continue;
                }
                throw new Exception("Helper not reachable after retry: " + e.getMessage(), e);
            } catch (ExceptionInRunningInstance e) {
                throw new Exception("Helper reported exception: " + e.getMessage(), e);
            } catch (InvalidResponseID e) {
                throw new Exception("Invalid response from helper: " + e.getMessage(), e);
            } catch (NoPortFileException e) {
                throw new Exception("Helper not ready (no port file): " + e.getMessage(), e);
            } catch (ErrorReadingResponseException e) {
                throw new Exception("Error reading helper response: " + e.getMessage(), e);
            }
        }
        startHelperLivenessWatcher(getHelperLockFile(), latch, errorRef);
        if (!latch.await(300000, TimeUnit.MILLISECONDS)) {
            throw new Exception("Timeout waiting for CMD_RESULT from helper (runAsLocalSystem)");
        }
        Exception ex = errorRef.get();
        if (ex != null) {
            throw ex;
        }
        ProcessOutput out = resultRef.get();
        if (out == null) {
            throw new Exception("No CMD_RESULT received from helper");
        }
        String stdoutStr = out.getStdOutString();
        String stderrStr = out.getErrOutString();
        if (opts != null && opts.getLogCallback() != null) {
            streamRemoteOutputToLogV3("runAsLocalSystem", stdoutStr != null ? stdoutStr : "", stderrStr != null ? stderrStr : "", opts.getLogCallback());
        }
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: runAsLocalSystem CMD_RESULT exitCode=" + out.getExitCode());
        }
        return out;
    }

    /**
     * Runs a static method as NT AUTHORITY\SYSTEM (LocalSystem). The method is invoked by reflection in a process running as SYSTEM.
     * Parameters and return value must be serializable (or null).
     *
     * @param clazz
     *            class that declares the static method
     * @param methodName
     *            name of the static method
     * @param paramTypes
     *            parameter types (e.g. String.class, Integer.TYPE)
     * @param args
     *            arguments (must match paramTypes)
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT})
     * @return the method's return value (serializable), or null, cast to T
     */
    public static <T> T runAsLocalSystem(final Class<?> clazz, final String methodName, final Class<?>[] paramTypes, final Object[] args, final TypeRef<T> resultType) throws Exception {
        String[] paramTypeNames = new String[paramTypes != null ? paramTypes.length : 0];
        if (paramTypes != null) {
            for (int i = 0; i < paramTypes.length; i++) {
                paramTypeNames[i] = paramTypes[i].getName();
            }
        }
        Object[] a = (args != null ? args : new Object[0]);
        return runAsLocalSystem(new MethodInvocationTask(clazz.getName(), methodName, paramTypeNames, a), resultType, ProcessOptions.DEFAULT);
    }

    /**
     * Runs a static method as NT AUTHORITY\SYSTEM (LocalSystem). Parameter types are inferred from the argument types (boxed types map to
     * primitives). For null arguments, Object.class is used. Parameters and return value must be serializable (or null).
     *
     * @param resultType
     *            type reference for the return value (e.g. {@link TypeRef#OBJECT})
     */
    public static <T> T runAsLocalSystem(final Class<?> clazz, final String methodName, final TypeRef<T> resultType, final Object... args) throws Exception {
        Class<?>[] paramTypes = inferParamTypes(args);
        Object[] a = (args != null ? args : new Object[0]);
        return runAsLocalSystem(clazz, methodName, paramTypes, a, resultType);
    }

    private static File getHelperLockDirectory() {
        if (effectiveHelperLockDir != null) {
            return effectiveHelperLockDir;
        }
        return getConfiguredHelperLockDirectory();
    }

    /** Returns the configured lock dir (env or Application.getHome()). Used to build fallback path when the default lock file is locked. */
    private static File getConfiguredHelperLockDirectory() {
        String envDir = System.getenv(ENV_HELPER_LOCK_DIR);
        if (envDir != null && envDir.trim().length() > 0) {
            return new File(envDir.trim());
        }
        return new File(Application.getHome());
    }

    /** When the default lock file could not be deleted (locked by another process), we use this fallback directory for our helper. */
    private static volatile File effectiveHelperLockDir = null;

    /**
     * Returns the app root the helper uses for its server ID. When set (e.g. via {@link #ENV_HELPER_APP_ROOT} by PostBuildRunner), clients
     * must use this same root for their client ID so multiple tests can open separate channels to the same helper.
     *
     * @return trimmed env value or null if not set
     */
    private static String getHelperAppRoot() {
        String env = System.getenv(ENV_HELPER_APP_ROOT);
        return (env != null && env.trim().length() > 0) ? env.trim() : null;
    }

    /**
     * Returns the random channel ID for helper communication. When set, clients use this as SingleAppInstance ID so parallel tests can each
     * open their own channel. Prefer this over {@link #getHelperAppRoot()} when available.
     *
     * @return env value, or channel ID stored when this process started the helper, or null
     */
    private static String getHelperChannelId() {
        String env = System.getenv(ENV_HELPER_CHANNEL_ID);
        if (env != null && env.trim().length() > 0) {
            return env.trim();
        }
        return helperChannelId;
    }

    /**
     * Creates a SingleAppInstance client for communicating with the admin helper. Uses a random {@link #getHelperChannelId() channel ID}
     * when available, else {@link #getHelperAppRoot()}, so that parallel tests can each open their own channel (connection) to the same
     * helper without client ID mismatch (GO_AWAY_INVALID_ID).
     */
    private static SingleAppInstance createHelperClient() {
        String idRoot = getHelperChannelId();
        if (idRoot == null) {
            idRoot = getHelperAppRoot();
        }
        return new HelperClientSingleAppInstance(AdminHelperProcess.APP_ID, getHelperLockDirectory(), null, idRoot);
    }

    private static File getHelperLockFile() {
        return new File(getHelperLockDirectory(), AdminHelperProcess.APP_ID + ".lock");
    }

    private static boolean isLockFileReady(File lockFile) {
        if (!lockFile.isFile() || lockFile.length() <= 1) {
            return false;
        }
        return true;
    }

    /**
     * Reads the port from the SingleAppInstance lock file (bytes after first byte). Returns -1 if unreadable or invalid.
     */
    private static int readPortFromLockFile(File lockFile) {
        if (!lockFile.isFile() || lockFile.length() <= 1) {
            return -1;
        }
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(lockFile);
            fis.getChannel().position(1);
            byte[] bytes = IO.readStream(-1, fis);
            if (bytes == null || bytes.length == 0) {
                return -1;
            }
            String portStr = new String(bytes, UTF8).split("\\r?\\n")[0].trim();
            return Integer.parseInt(portStr, 10);
        } catch (Throwable t) {
            return -1;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Returns true if the helper is actually listening on the port written in the lock file (avoids "Connection refused" when reusing a
     * stale lock).
     */
    private static boolean isHelperPortReachable(File lockFile) {
        int port = readPortFromLockFile(lockFile);
        if (port <= 0 || port > 65535) {
            return false;
        }
        Socket socket = null;
        try {
            socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 2000);
            return true;
        } catch (Throwable t) {
            return false;
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Starts a daemon thread that periodically checks whether the helper is still reachable. If the helper process terminates unexpectedly,
     * sets errorRef and counts down the latch so the waiting caller fails fast instead of waiting for the full timeout. Stops when the
     * latch has already been counted down (normal response received).
     */
    private static void startHelperLivenessWatcher(final File lockFile, final CountDownLatch latch, final AtomicReference<Exception> errorRef) {
        Thread watcher = new Thread(new Runnable() {
            @Override
            public void run() {
                while (latch.getCount() > 0) {
                    try {
                        Thread.sleep(HELPER_LIVENESS_POLL_MS);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        return;
                    }
                    if (latch.getCount() == 0) {
                        return;
                    }
                    if (!isHelperPortReachable(lockFile)) {
                        errorRef.set(new Exception("Helper process terminated unexpectedly"));
                        latch.countDown();
                        return;
                    }
                }
            }
        }, "AdminExecuter-HelperLivenessWatcher");
        watcher.setDaemon(true);
        watcher.start();
    }

    private static void logVerbose(String msg) {
        if (isVerbose()) {
            LogV3.info("AdminExecuter [verbose]: " + msg);
        }
    }

    /**
     * Ensures the elevated helper process is running (one UAC prompt if needed). When env vars {@link #ENV_HELPER_LOCK_DIR} and
     * {@link #ENV_HELPER_PRIVATE_KEY_BASE64} are set (e.g. by PostBuildRunner), uses the existing helper and does not start a new one. Call
     * this from PostBuildRunner main process once before launching tests that depend on AdminExecuter.
     */
    public static void ensureHelperRunning() throws Exception {
        String envLockDir = System.getenv(ENV_HELPER_LOCK_DIR);
        String envPrivateKey = System.getenv(ENV_HELPER_PRIVATE_KEY_BASE64);
        if (envLockDir != null && envLockDir.trim().length() > 0 && envPrivateKey != null && envPrivateKey.trim().length() > 0) {
            try {
                KeyPair pair = HelperChannelCrypto.keyPairFromPrivateKey(HelperChannelCrypto.decodePrivateKeyFromBase64(envPrivateKey.trim()));
                if (pair != null) {
                    helperKeyPair = pair;
                    File envLockFile = new File(envLockDir.trim(), AdminHelperProcess.APP_ID + ".lock");
                    if (isLockFileReady(envLockFile) && isHelperPortReachable(envLockFile)) {
                        logVerbose("ensureHelperRunning: using existing helper from env (lockDir=" + envLockDir + ")");
                        return;
                    }
                    LogV3.warning("AdminExecuter: env helper not reachable (stale lock or helper gone), will start own helper");
                }
            } catch (Throwable t) {
                LogV3.warning("AdminExecuter: failed to use helper from env, will start own: " + t.getMessage());
            }
        }
        ensureHelperRunningInternal();
    }

    private static void ensureHelperRunningInternal() throws Exception {
        File lockDir = getHelperLockDirectory();
        File lockFile = new File(lockDir, AdminHelperProcess.APP_ID + ".lock");
        logVerbose("ensureHelperRunning: lockDir=" + lockDir.getAbsolutePath() + " lockFile=" + lockFile.getAbsolutePath() + " exists=" + lockFile.exists() + " length=" + (lockFile.exists() ? lockFile.length() : -1));
        if (isLockFileReady(lockFile)) {
            if (isHelperPortReachable(lockFile)) {
                LogV3.info("AdminExecuter: reusing existing helper. Running task.");
                logVerbose("ensureHelperRunning: reusing existing helper (lock file ready, port reachable)");
                return;
            }
            // Lock file exists but port not reachable yet - helper may still be binding. Wait and retry before treating as stale.
            long retryDeadline = System.currentTimeMillis() + 15000;
            while (System.currentTimeMillis() < retryDeadline) {
                try {
                    Thread.sleep(HELPER_POLL_MS);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new Exception("Interrupted while waiting for helper port", e);
                }
                if (isHelperPortReachable(lockFile)) {
                    LogV3.info("AdminExecuter: reusing existing helper. Running task.");
                    logVerbose("ensureHelperRunning: helper port reachable after wait");
                    return;
                }
            }
            logVerbose("ensureHelperRunning: lock file exists but helper port not reachable after wait (stale?), starting new helper");
            if (!lockFile.delete()) {
                LogV3.warning("AdminExecuter: could not delete stale lock file " + lockFile.getAbsolutePath() + " - " + WindowsUtils.getLocksOnPath(lockFile));
                // Use a fallback lock directory so our new helper can create its lock file there (other process still holds the default
                // file).
                File fallbackDir = new File(getConfiguredHelperLockDirectory(), "AWTestAdminHelper_" + UniqueAlltimeID.next());
                if (!fallbackDir.mkdirs()) {
                    throw new Exception("Stale lock file is locked by another process (e.g. " + WindowsUtils.getLocksOnPath(lockFile) + ") and could not create fallback dir " + fallbackDir.getAbsolutePath());
                }
                effectiveHelperLockDir = fallbackDir;
                lockDir = effectiveHelperLockDir;
                lockFile = new File(lockDir, AdminHelperProcess.APP_ID + ".lock");
                LogV3.info("AdminExecuter: using fallback lock dir " + lockDir.getAbsolutePath());
            }
        }
        String appRootAbs = Application.getRoot(SingleAppInstance.class);
        String classpath = System.getProperty("java.class.path");
        String javaBin = CrossSystem.getJavaBinary();
        String lockDirAbs = lockDir.getAbsolutePath();
        boolean debugLogs = "true".equalsIgnoreCase(System.getProperty(RUN_AS_ADMIN_DEBUG_HELPER_LOGS));
        boolean showDebugWindow = debugLogs || "true".equalsIgnoreCase(System.getProperty(RUN_AS_ADMIN_SHOW_DEBUG_WINDOW));
        boolean verbose = isVerbose();
        // Use a random channel ID so parallel tests can each open their own channel to this helper
        helperChannelId = String.valueOf(UniqueAlltimeID.next());
        String[] helperCmd = new String[] { javaBin, "-cp", classpath, AdminHelperProcess.class.getName(), "-lockDir", lockDirAbs, "-channelId", helperChannelId, "-appRoot", appRootAbs };
        if (verbose) {
            helperCmd = addHelperArg(helperCmd, "-verbose");
        }
        if (showDebugWindow) {
            helperCmd = addHelperArg(helperCmd, "-showDebugWindow");
        }
        File helperDebugLogDir = null;
        if (showDebugWindow) {
            helperDebugLogDir = Application.getTempResource("AdminHelperLogs_" + UniqueAlltimeID.next());
            helperDebugLogDir.mkdirs();
            helperCmd = addHelperArg(helperCmd, "-debugLogDir");
            helperCmd = addHelperArg(helperCmd, helperDebugLogDir.getAbsolutePath());
        }
        helperKeyPair = HelperChannelCrypto.generateKeyPair();
        String publicKeyBase64 = HelperChannelCrypto.encodePublicKeyToBase64(helperKeyPair.getPublic());
        helperCmd = addHelperArg(addHelperArg(helperCmd, "-publicKeyBase64"), publicKeyBase64);
        logVerbose("helper cmd: javaBin=" + javaBin + " classpathLen=" + (classpath != null ? classpath.length() : 0) + " appRoot=" + appRootAbs + " lockDir=" + lockDirAbs + " debugLogs=" + debugLogs + " showDebugWindow=" + showDebugWindow + " verbose=" + verbose);
        JNAProcessInfo pinfo = null;
        try {
            LogV3.info("AdminExecuter: starting elevated helper (UAC prompt may appear)...");
            pinfo = WindowsUtils.startElevatedProcess(helperCmd, null, false);
            logVerbose("startElevatedProcess returned pid=" + pinfo.getPid());
            if (helperDebugLogDir != null) {
                startHelperLogReaderThread(helperDebugLogDir, pinfo);
            }
            // startHelperPollerThread(pinfo.getPid());
        } catch (Throwable t) {
            logVerbose("startElevatedProcess failed: " + t.getClass().getName() + " " + t.getMessage());
            throw new Exception("Failed to start elevated helper (UAC): " + t.getMessage(), t);
        }
        long deadline = System.currentTimeMillis() + HELPER_WAIT_MS;
        int pollCount = 0;
        while (System.currentTimeMillis() < deadline) {
            if (isLockFileReady(lockFile)) {
                logVerbose("lock file ready after " + pollCount + " polls");
                break;
            }
            pollCount++;
            if (isVerbose() && pollCount <= 5 || pollCount % 25 == 0) {
                logVerbose("waiting for helper poll=" + pollCount + " lockFile.exists=" + lockFile.exists() + " length=" + (lockFile.exists() ? lockFile.length() : -1));
            }
            try {
                Thread.sleep(HELPER_POLL_MS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                if (pinfo != null) {
                    pinfo.close();
                }
                throw new Exception("Interrupted while waiting for helper", e);
            }
        }
        if (!isLockFileReady(lockFile)) {
            logVerbose("helper did not become ready; pollCount=" + pollCount + " lockFile.exists=" + lockFile.exists());
            if (pinfo != null) {
                pinfo.close();
            }
            throw new Exception("Helper did not become ready within " + (HELPER_WAIT_MS / 1000) + "s (confirm UAC)");
        }
        // Wait for helper to bind and become reachable (avoids race where lock file was stale or just written)
        long reachableDeadline = System.currentTimeMillis() + 10000;
        while (!isHelperPortReachable(lockFile) && System.currentTimeMillis() < reachableDeadline) {
            try {
                Thread.sleep(HELPER_POLL_MS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                if (pinfo != null) {
                    pinfo.close();
                }
                throw new Exception("Interrupted while waiting for helper port", e);
            }
        }
        if (!isHelperPortReachable(lockFile)) {
            if (pinfo != null) {
                pinfo.close();
            }
            throw new Exception("Helper lock file ready but port not reachable within 10s (helper may have exited)");
        }
        if (pinfo != null) {
            pinfo.close();
        }
        LogV3.info("AdminExecuter: helper ready. Running task.");
        logVerbose("ensureHelperRunning finished successfully");
    }

    /**
     * Returns environment variables to pass to child test processes that use AdminExecuter, so they reuse the same helper (lock dir and
     * private key). Only returns a non-null map when this process started the helper (has key pair and did not receive it via env). Used by
     * PostBuildRunner after calling {@link #ensureHelperRunning()}.
     *
     * @return map with {@link #ENV_HELPER_LOCK_DIR}, {@link #ENV_HELPER_PRIVATE_KEY_BASE64}, {@link #ENV_HELPER_APP_ROOT}, and
     *         {@link #ENV_HELPER_CHANNEL_ID} (when this process started the helper), or null if not applicable
     */
    public static Map<String, String> getHelperConnectionEnv() {
        if (helperKeyPair == null) {
            return null;
        }
        if (System.getenv(ENV_HELPER_PRIVATE_KEY_BASE64) != null) {
            return null;
        }
        File lockDir = getHelperLockDirectory();
        String privateKeyBase64 = HelperChannelCrypto.encodePrivateKeyToBase64(helperKeyPair.getPrivate());
        if (lockDir == null || privateKeyBase64 == null) {
            return null;
        }
        Map<String, String> env = new java.util.HashMap<String, String>();
        env.put(ENV_HELPER_LOCK_DIR, lockDir.getAbsolutePath());
        env.put(ENV_HELPER_PRIVATE_KEY_BASE64, privateKeyBase64);
        env.put(ENV_HELPER_APP_ROOT, Application.getRoot(SingleAppInstance.class));
        if (helperChannelId != null) {
            env.put(ENV_HELPER_CHANNEL_ID, helperChannelId);
        }
        return env;
    }

    private static String[] addHelperArg(String[] cmd, String arg) {
        String[] n = new String[cmd.length + 1];
        System.arraycopy(cmd, 0, n, 0, cmd.length);
        n[cmd.length] = arg;
        return n;
    }
    // private static void startHelperPollerThread(final int helperPid) {
    // Thread poller = new Thread(new Runnable() {
    // @Override
    // public void run() {
    // final int pollIntervalMs = 10000;
    // while (isHelperProcessAlive(helperPid)) {
    // LogV3.info("Admin helper (PID " + helperPid + ") still running");
    // try {
    // Thread.sleep(pollIntervalMs);
    // } catch (InterruptedException e) {
    // Thread.currentThread().interrupt();
    // return;
    // }
    // }
    // LogV3.info("Admin helper (PID " + helperPid + ") has exited");
    // }
    // }, "AdminHelper-Poller");
    // poller.setDaemon(true);
    // poller.start();
    // }

    /**
     * Starts a daemon thread that reads stdout.txt and stderr.txt from the helper's debug log dir and forwards each line to LogV3. Stops
     * when the helper process exits. So the parent (test) sees the helper's logs in its console.
     */
    private static void startHelperLogReaderThread(final File logDir, final JNAProcessInfo pinfo) {
        final File stdoutFile = new File(logDir, "stdout.txt");
        final File stderrFile = new File(logDir, "stderr.txt");
        final String prefix = "runAsAdmin [Helper]";
        Thread reader = new Thread(new Runnable() {
            long stdoutPos = 0;
            long stderrPos = 0;

            @Override
            public void run() {
                while (true) {
                    List<ProcessInfo> list;
                    try {
                        list = ProcessHandlerFactory.getProcessHandler().listByProcessInfo(pinfo);
                        if (list.size() == 0) {
                            break;
                        }
                        if (list.get(0).getExitCode() != null) {
                            break;
                        }
                        try {
                            stdoutPos = readAndLogNewLines(stdoutFile, stdoutPos, UTF8, prefix + " [stdout]", false);
                            stderrPos = readAndLogNewLines(stderrFile, stderrPos, UTF8, prefix + " [stderr]", true);
                        } catch (Throwable t) {
                            // ignore
                        }
                        Thread.sleep(500);
                    } catch (NotSupportedException e) {
                        LogV3.log(e);
                        return;
                    } catch (IOException e) {
                        LogV3.log(e);
                        return;
                    } catch (InterruptedException e) {
                        return;
                    }
                }
                // Final read
                readAndLogNewLines(stdoutFile, stdoutPos, UTF8, prefix + " [stdout]", false);
                readAndLogNewLines(stderrFile, stderrPos, UTF8, prefix + " [stderr]", true);
            }
        }, "AdminHelper-LogReader");
        reader.setDaemon(true);
        reader.start();
    }

    /**
     * Reads new bytes from file from the given position, splits into lines, logs each non-empty line to LogV3, and returns the new file
     * position (after the last complete line).
     */
    private static long readAndLogNewLines(File file, long fromPosition, Charset charset, String logPrefix, boolean asStderr) {
        if (file == null || !file.isFile()) {
            return fromPosition;
        }
        RandomAccessFile raf = null;
        try {
            raf = new RandomAccessFile(file, "r");
            long len = raf.length();
            if (len <= fromPosition) {
                return fromPosition;
            }
            raf.seek(fromPosition);
            int toRead = (int) Math.min(len - fromPosition, 65536);
            byte[] buf = new byte[toRead];
            int n = raf.read(buf);
            raf.close();
            raf = null;
            if (n <= 0) {
                return fromPosition;
            }
            String chunk = new String(buf, 0, n, charset);
            int lastNewline = chunk.lastIndexOf('\n');
            if (lastNewline >= 0) {
                String fullLines = chunk.substring(0, lastNewline + 1);
                String[] lines = fullLines.split("\\r?\\n");
                for (int i = 0; i < lines.length; i++) {
                    String line = lines[i] != null ? lines[i].trim() : "";
                    if (line.length() > 0) {
                        if (asStderr) {
                            LogV3.warning(logPrefix + ": " + line);
                        } else {
                            LogV3.info(logPrefix + ": " + line);
                        }
                    }
                }
                return fromPosition + lastNewline + 1;
            }
            return fromPosition;
        } catch (Throwable t) {
            return fromPosition;
        } finally {
            if (raf != null) {
                try {
                    raf.close();
                } catch (IOException e) {
                }
            }
        }
    }
}
