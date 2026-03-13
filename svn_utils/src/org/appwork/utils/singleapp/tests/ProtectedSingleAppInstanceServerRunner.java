/**
 * Helper for {@link ProtectedSingleAppInstanceTest} (b): run as elevated (e.g. via AdminExecuter.runAsAdmin).
 * Starts a {@link ProtectedSingleAppInstance} server in a background thread and returns the port; use
 * {@link #stopServer(String)} to stop it. For use with runAsAdmin(Class, "startServerInBackgroundAndReturnPort", TypeRef.INT, lockDirPath, appID).
 * <p>
 * Legacy: main(String[] args) still supports lockDir, appID, portFile, stopFile for process-based runAsLocalSystem.
 */
package org.appwork.utils.singleapp.tests;

import java.io.File;
import java.io.FileInputStream;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.IO;
import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.ProtectedSingleAppInstance;

/**
 * Runs ProtectedSingleAppInstance server elevated; test (non-elevated) connects and expects rejection.
 */
public class ProtectedSingleAppInstanceServerRunner {
    private static final ConcurrentHashMap<String, AtomicReference<ProtectedSingleAppInstance>> SERVER_BY_APP_ID = new ConcurrentHashMap<String, AtomicReference<ProtectedSingleAppInstance>>();

    /**
     * Starts a ProtectedSingleAppInstance server in a background thread (in the task's JVM, which runs in a separate process) and returns a
     * {@link ResultWithCancel} with the port and cancel info. Use
     * {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, ProcessOptions, Object...)} with a
     * {@link ProcessOptions#cancelCallback(CancelCallback)} to get a runnable that calls {@link #stopServer(String)}.
     *
     * @param lockDirPath
     *            directory path for the SingleAppInstance lock file
     * @param appID
     *            application ID for the lock
     * @return result with port and cancel info (stopServer(appID))
     */
    public static int startServerInBackgroundAndReturnPort(final String lockDirPath, final String appID) throws Exception {
        final File lockDir = new File(lockDirPath);
        lockDir.mkdirs();
        final AtomicReference<ProtectedSingleAppInstance> serverRef = new AtomicReference<ProtectedSingleAppInstance>(null);
        SERVER_BY_APP_ID.put(appID, serverRef);
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(org.appwork.utils.singleapp.ResponseSender sender, String[] message) {
            }
        };
        final ProtectedSingleAppInstance server = new ProtectedSingleAppInstance(appID, lockDir, listener);
        serverRef.set(server);
        server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
        Thread serverThread = new Thread("ProtectedSingleAppInstanceServer") {
            @Override
            public void run() {
                try {
                    server.start(null, "");
                    Thread.sleep(10000000000l);
                } catch (Exception e) {
                    LogV3.log(e);
                }
            }
        };
        serverThread.setDaemon(true);
        LogV3.info("Start Daemon");
        serverThread.start();
        int port = waitForPortInLockFile(lockDir, appID);
        LogV3.info("Return PORT");
        return port;
    }

    /**
     * Stops the server previously started with {@link #startServerInBackgroundAndReturnPort(String, String)}. For use with
     * runAsAdmin(ProtectedSingleAppInstanceServerRunner.class, "stopServer", TypeRef.OBJECT, appID).
     *
     * @param appID
     *            same appID passed to startServerInBackgroundAndReturnPort
     */
    public static void stopServer(final String appID) {
        AtomicReference<ProtectedSingleAppInstance> ref = SERVER_BY_APP_ID.remove(appID);
        if (ref != null) {
            ProtectedSingleAppInstance s = ref.get();
            if (s != null) {
                Thread exitThread = s.exit(true);
                if (exitThread != null) {
                    try {
                        exitThread.join(5000);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
        }
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.err.println("Usage: ProtectedSingleAppInstanceServerRunner <lockDir> <appID> <portFile> <stopFile>");
            System.exit(2);
        }
        File lockDir = new File(args[0]);
        String appID = args[1];
        File portFile = new File(args[2]);
        File stopFile = new File(args[3]);
        lockDir.mkdirs();
        final AtomicReference<ProtectedSingleAppInstance> serverRef = new AtomicReference<ProtectedSingleAppInstance>(null);
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(org.appwork.utils.singleapp.ResponseSender sender, String[] message) {
            }
        };
        final ProtectedSingleAppInstance server = new ProtectedSingleAppInstance(appID, lockDir, listener);
        serverRef.set(server);
        server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
        Thread serverThread = new Thread("ProtectedSingleAppInstanceServer") {
            @Override
            public void run() {
                try {
                    server.start(null, "");
                } catch (Throwable t) {
                    t.printStackTrace();
                    System.exit(4);
                }
            }
        };
        serverThread.setDaemon(false);
        serverThread.start();
        int port = waitForPortInLockFile(lockDir, appID);
        IO.writeStringToFile(portFile, String.valueOf(port) + "\r\n");
        long deadline = System.currentTimeMillis() + 60000;
        while (!stopFile.exists() && System.currentTimeMillis() < deadline) {
            Thread.sleep(200);
        }
        ProtectedSingleAppInstance s = serverRef.get();
        if (s != null) {
            Thread exitThread = s.exit(true);
            if (exitThread != null) {
                exitThread.join(5000);
            }
        }
    }

    private static int waitForPortInLockFile(File lockDir, String appID) throws Exception {
        File lockFile = new File(lockDir, appID + ".lock");
        long deadline = System.currentTimeMillis() + 15000;
        while (System.currentTimeMillis() < deadline) {
            if (lockFile.isFile() && lockFile.length() > 2) {
                FileInputStream fis = null;
                try {
                    fis = new FileInputStream(lockFile);
                    fis.getChannel().position(1);
                    byte[] buf = new byte[32];
                    int n = fis.read(buf);
                    if (n > 0) {
                        String s = new String(buf, 0, n, "UTF-8").trim().split("\r?\n")[0].trim();
                        if (s.matches("\\d+")) {
                            return Integer.parseInt(s, 10);
                        }
                    }
                } finally {
                    if (fis != null) {
                        fis.close();
                    }
                }
            }
            Thread.sleep(50);
        }
        throw new IllegalStateException("Lock file did not contain port in time");
    }
}
