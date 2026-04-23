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
import java.io.Serializable;
import java.nio.charset.Charset;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.os.CrossSystem;

/**
 * Entry point for running an {@link ElevatedTestTask} in a subprocess. Used by {@link AdminHelperProcess} for RUN_TASK. Reads the
 * serialized task from task.bin in the given directory, runs it, writes the hex-encoded {@link AdminTaskResultWrapper} to result.hex. If a
 * file "keep_running" with content "true" exists in the temp dir (written by the helper when {@link ProcessOptions#isKeepRunning()} is
 * true), the JVM does not exit after writing the result so that background threads (e.g. a server) keep running; the process can be
 * terminated later via {@link ProcessOptions#terminate()}.
 * <p>
 * Usage: {@code java -cp <classpath> RunTaskAsSystemMain <tempDir> [taskId]}
 */
public final class RunTaskAsSystemMain {
    private static final Charset UTF8              = Charset.forName("UTF-8");
    private static final String  TASK_BIN          = "task.bin";
    /** When this file exists with content "true", do not exit after writing result so background threads (e.g. server) keep running. */
    private static final String  KEEP_RUNNING_FILE = "keep_running";

    public static void main(String[] args) {
        BuildDecisions.setEnabled(false);
        if (!CrossSystem.isWindows()) {
            System.err.println("RunTaskAsSystemMain is Windows-only");
            System.exit(1);
        }
        if (args == null || args.length < 1) {
            System.err.println("Usage: RunTaskAsSystemMain <tempDir> [taskId]");
            System.exit(1);
        }
        File tempDir = new File(args[0]);
        String taskId = args.length >= 2 && args[1] != null && args[1].trim().length() > 0 ? args[1].trim() : "unknown";
        Application.setApplication("adminexecute." + taskId);
        ensureLogV3MirrorsToStdStreamsForRemoteParent();
        if (!tempDir.isDirectory()) {
            System.err.println("RunTaskAsSystemMain: Not a directory: " + tempDir);
            System.exit(1);
        }
        System.err.println("RunTaskAsSystemMain: tempDir=" + tempDir.getAbsolutePath() + " taskId=" + taskId);
        try {
            long pid = CrossSystem.getPID();
            if (pid > 0) {
                IO.writeToFile(new File(tempDir, "pid.txt"), Long.toString(pid).getBytes(UTF8));
            }
        } catch (Throwable t) {
            // ignore
        }
        File taskFile = new File(tempDir, TASK_BIN);
        if (!taskFile.isFile()) {
            System.err.println("RunTaskAsSystemMain: Missing task file: " + taskFile);
            System.exit(1);
        }
        byte[] taskBytes = readFile(taskFile);
        if (taskBytes == null || taskBytes.length == 0) {
            System.err.println("RunTaskAsSystemMain: Empty task file");
            System.exit(1);
        }
        System.err.println("RunTaskAsSystemMain: task.bin size=" + taskBytes.length + ", deserializing");
        ElevatedTestTask task = null;
        try {
            task = deserializeTask(taskBytes);
        } catch (Throwable t) {
            System.err.println("RunTaskAsSystemMain: Deserialize task failed: " + Exceptions.getStackTrace(t));
            System.exit(1);
        }
        if (task == null) {
            System.err.println("RunTaskAsSystemMain: task is null after deserialize");
            System.exit(1);
        }
        final String resultHexFile = "result.hex";
        try {
            System.err.println("RunTaskAsSystemMain: running task");
            Serializable result = task.run();
            System.err.println("RunTaskAsSystemMain: task.run() returned, writing result.hex");
            flushStdStreams();
            AdminTaskResultWrapper wrapper = new AdminTaskResultWrapper(result, "", "");
            byte[] wrapperBytes = serializeWrapper(wrapper);
            String hex = HexFormatter.byteArrayToHex(wrapperBytes);
            IO.writeToFile(new File(tempDir, resultHexFile), hex.getBytes(UTF8));
            System.err.println("RunTaskAsSystemMain: result.hex written, size=" + hex.length());
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
            System.err.println("RunTaskAsSystemMain: Task execution failed: " + stack);
            flushStdStreams();
            try {
                AdminTaskResultWrapper failureWrapper = new AdminTaskResultWrapper(null, "", stack, stack);
                byte[] wrapperBytes = serializeWrapper(failureWrapper);
                String hex = HexFormatter.byteArrayToHex(wrapperBytes);
                IO.writeToFile(new File(tempDir, resultHexFile), hex.getBytes(UTF8));
            } catch (Throwable writeEx) {
                System.err.println("RunTaskAsSystemMain: failed to write result.hex with exception: " + writeEx.getMessage());
            }
            System.exit(1);
        }
    }

    /**
     * Replaces logging with a {@link SimpleLoggerFactory} that writes to stdout/stderr only, so helper capture (pipe or redirected files)
     * sees all org.appwork.loggingv3.LogV3 lines. Avoids {@link org.appwork.loggingv3.PreInitLoggerFactory} buffering and keeps streams aligned with
     * {@link org.appwork.loggingv3.simple.sink.LogToStdOutSink} after {@link Application} may have wrapped System.out.
     */
    private static void ensureLogV3MirrorsToStdStreamsForRemoteParent() {
        try {
            new SimpleLoggerFactory().initDefaults().set();
        } catch (Throwable t) {
            // ignore
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
            return org.appwork.utils.IO.readStream(-1, fis);
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
