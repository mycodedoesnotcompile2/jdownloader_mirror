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
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Serializable;
import java.nio.charset.Charset;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.windows.execute.RunAsHelper;

/**
 * Entry point for running an {@link ElevatedTestTask} in a subprocess. Used by {@link AdminHelperProcess} for RUN_TASK and RUN_AS_USER_TASK.
 * Reads the serialized task from task.bin in the given directory, runs it, writes the hex-encoded {@link AdminTaskResultWrapper} to
 * result.hex.
 * <p>
 * Usage: {@code java -cp <classpath> RunSerializedTaskMain <tempDir> [taskId]}
 * <p>
 * {@link AdminHelperProcess} sets {@value #SYSPROP_RUN_SERIALIZED_ORIGIN}={@value #RUN_SERIALIZED_ORIGIN_RUN_AS_USER} for
 * {@code RUN_AS_USER_TASK} children so {@link AdminExecuter#runAsAdmin} can elevate the <strong>same</strong> Windows user via
 * {@link org.appwork.utils.os.WindowsUtils#startElevatedProcess} instead of the shared elevated helper (different principal).
 */
public final class RunSerializedTaskMain {
    private static final Charset UTF8              = Charset.forName("UTF-8");
    private static final String  TASK_BIN          = "task.bin";
    private static final String  KEEP_RUNNING_FILE = "keep_running";
    /**
     * When {@link AdminHelperProcess} starts this main class for {@code RUN_AS_USER_TASK}, it passes
     * {@code -D}{@value #SYSPROP_RUN_SERIALIZED_ORIGIN}={@value #RUN_SERIALIZED_ORIGIN_RUN_AS_USER}.
     */
    public static final String  SYSPROP_RUN_SERIALIZED_ORIGIN   = "awtest.runSerializedOrigin";
    public static final String  RUN_SERIALIZED_ORIGIN_RUN_AS_USER = "RUN_AS_USER";
    /** Set on JVMs started by the elevated helper for {@code RUN_TASK} ({@link org.appwork.testframework.executer.AdminHelperProcess}). */
    public static final String  RUN_SERIALIZED_ORIGIN_RUN_TASK     = "RUN_TASK";

    /**
     * @return true if this JVM was started as the {@code RUN_AS_USER_TASK} serialized-task child (not the elevated helper's {@code RUN_TASK}
     *         child).
     */
    public static boolean isLaunchedFromRunAsUserIpcTask() {
        return RUN_SERIALIZED_ORIGIN_RUN_AS_USER.equals(System.getProperty(SYSPROP_RUN_SERIALIZED_ORIGIN));
    }

    /**
     * @return true if this JVM was started by the elevated helper for {@code RUN_TASK} (may call {@link AdminExecuter#runAsUser} in-process
     *         via {@link RunTaskAsUserLauncher} instead of starting a second UAC helper).
     */
    public static boolean isLaunchedFromElevatedHelperTask() {
        return RUN_SERIALIZED_ORIGIN_RUN_TASK.equals(System.getProperty(SYSPROP_RUN_SERIALIZED_ORIGIN));
    }

    public static void main(String[] args) {
        final PrintStream originalOut = System.out;
        final PrintStream originalErr = System.err;
        final ByteArrayOutputStream capturedOut = new ByteArrayOutputStream();
        final ByteArrayOutputStream capturedErr = new ByteArrayOutputStream();
        installStreamCapture(originalOut, originalErr, capturedOut, capturedErr);
        BuildDecisions.setEnabled(false);
        if (!CrossSystem.isWindows()) {
            System.err.println("RunSerializedTaskMain is Windows-only");
            System.exit(1);
        }
        if (args == null || args.length < 1) {
            System.err.println("Usage: RunSerializedTaskMain <tempDir> [taskId]");
            System.exit(1);
        }
        File tempDir = new File(args[0]);
        String taskId = args.length >= 2 && args[1] != null && args[1].trim().length() > 0 ? args[1].trim() : "unknown";
        Application.setApplication("adminexecute." + taskId);
        ensureLogV3MirrorsToStdStreamsForRemoteParent();
        if (!tempDir.isDirectory()) {
            System.err.println("RunSerializedTaskMain: Not a directory: " + tempDir);
            System.exit(1);
        }
        System.err.println("RunSerializedTaskMain: tempDir=" + tempDir.getAbsolutePath() + " taskId=" + taskId);
        try {
            long pid = CrossSystem.getPID();
            if (pid > 0) {
                IO.writeToFile(new File(tempDir, "pid.txt"), Long.toString(pid).getBytes(UTF8));
            }
        } catch (Throwable t) {
        }
        File taskFile = new File(tempDir, TASK_BIN);
        if (!taskFile.isFile()) {
            System.err.println("RunSerializedTaskMain: Missing task file: " + taskFile);
            System.exit(1);
        }
        byte[] taskBytes = readFile(taskFile);
        if (taskBytes == null || taskBytes.length == 0) {
            System.err.println("RunSerializedTaskMain: Empty task file");
            System.exit(1);
        }
        System.err.println("RunSerializedTaskMain: task.bin size=" + taskBytes.length + ", deserializing");
        ElevatedTestTask task = null;
        try {
            task = deserializeTask(taskBytes);
        } catch (Throwable t) {
            System.err.println("RunSerializedTaskMain: Deserialize task failed: " + Exceptions.getStackTrace(t));
            System.exit(1);
        }
        if (task == null) {
            System.err.println("RunSerializedTaskMain: task is null after deserialize");
            System.exit(1);
        }
        final String resultHexFile = "result.hex";
        try {
            System.err.println("RunSerializedTaskMain: running task");
            RunAsHelper.logCallerTokenDiagnosticsForTokenLaunch("RunSerializedTaskMain_before_ElevatedTestTask.run");
            Serializable result = task.run();
            System.err.println("RunSerializedTaskMain: task.run() returned, writing result.hex");
            flushStdStreams();
            String outText = toUtf8String(capturedOut);
            String errText = toUtf8String(capturedErr);
            AdminTaskResultWrapper wrapper = new AdminTaskResultWrapper(result, outText, errText);
            byte[] wrapperBytes = serializeWrapper(wrapper);
            String hex = HexFormatter.byteArrayToHex(wrapperBytes);
            IO.writeToFile(new File(tempDir, resultHexFile), hex.getBytes(UTF8));
            System.err.println("RunSerializedTaskMain: result.hex written, size=" + hex.length());
            if (isKeepRunning(tempDir)) {
                try {
                    Thread.sleep(Long.MAX_VALUE);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            } else {
                System.exit(0);
            }
        } catch (Throwable t) {
            String stack = Exceptions.getStackTrace(t);
            System.err.println("RunSerializedTaskMain: Task execution failed: " + stack);
            flushStdStreams();
            try {
                String outText = toUtf8String(capturedOut);
                String errText = appendText(toUtf8String(capturedErr), stack);
                AdminTaskResultWrapper failureWrapper = new AdminTaskResultWrapper(null, outText, errText, stack);
                byte[] wrapperBytes = serializeWrapper(failureWrapper);
                String hex = HexFormatter.byteArrayToHex(wrapperBytes);
                IO.writeToFile(new File(tempDir, resultHexFile), hex.getBytes(UTF8));
            } catch (Throwable writeEx) {
                System.err.println("RunSerializedTaskMain: failed to write result.hex with exception: " + writeEx.getMessage());
            }
            System.exit(1);
        }
    }

    private static void ensureLogV3MirrorsToStdStreamsForRemoteParent() {
        try {
            new SimpleLoggerFactory().initDefaults().set();
        } catch (Throwable t) {
        }
    }

    private static void installStreamCapture(PrintStream originalOut, PrintStream originalErr, ByteArrayOutputStream capturedOut, ByteArrayOutputStream capturedErr) {
        try {
            System.setOut(new PrintStream(new TeeOutputStream(originalOut, capturedOut), true, UTF8.name()));
        } catch (Throwable t) {
        }
        try {
            System.setErr(new PrintStream(new TeeOutputStream(originalErr, capturedErr), true, UTF8.name()));
        } catch (Throwable t) {
        }
    }

    private static String toUtf8String(ByteArrayOutputStream baos) {
        if (baos == null || baos.size() == 0) {
            return "";
        }
        return new String(baos.toByteArray(), UTF8);
    }

    private static String appendText(String a, String b) {
        String left = a != null ? a : "";
        String right = b != null ? b : "";
        if (left.length() == 0) {
            return right;
        }
        if (right.length() == 0) {
            return left;
        }
        if (left.endsWith("\n")) {
            return left + right;
        }
        return left + "\n" + right;
    }

    private static final class TeeOutputStream extends OutputStream {
        private final OutputStream a;
        private final OutputStream b;

        private TeeOutputStream(OutputStream a, OutputStream b) {
            this.a = a;
            this.b = b;
        }

        @Override
        public void write(int bt) throws IOException {
            a.write(bt);
            b.write(bt);
        }

        @Override
        public void write(byte[] bytes, int off, int len) throws IOException {
            a.write(bytes, off, len);
            b.write(bytes, off, len);
        }

        @Override
        public void flush() throws IOException {
            a.flush();
            b.flush();
        }
    }

    private static void flushStdStreams() {
        try {
            System.out.flush();
        } catch (Throwable t) {
        }
        try {
            System.err.flush();
        } catch (Throwable t) {
        }
    }

    private static boolean isKeepRunning(File tempDir) {
        File f = new File(tempDir, KEEP_RUNNING_FILE);
        if (!f.isFile()) {
            return false;
        }
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(f);
            String s = new String(IO.readStream(-1, fis), UTF8);
            return "true".equalsIgnoreCase(s != null ? s.trim() : "");
        } catch (Throwable t) {
            return false;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                }
            }
        }
    }

    private static byte[] readFile(File file) {
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            return IO.readStream(-1, fis);
        } catch (IOException e) {
            return null;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
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

    private static byte[] serializeWrapper(AdminTaskResultWrapper wrapper) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = null;
        try {
            oos = new ObjectOutputStream(baos);
            oos.writeObject(wrapper);
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
}
