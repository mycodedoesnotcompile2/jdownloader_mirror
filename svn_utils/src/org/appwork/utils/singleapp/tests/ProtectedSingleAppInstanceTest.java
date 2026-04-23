/**
 * AWTest for {@link ProtectedSingleAppInstance}: verifies that incoming connections are rejected when
 * (a) the client is a different Windows user (e.g. LocalSystem), and
 * (b) the server runs elevated and the client is non-elevated.
 * <p>
 * Both tests run on Windows only. Test (a) uses
 * {@link AdminExecuter#runAsLocalSystem(Class, String, TypeRef, Object...)}
 * to run the client as SYSTEM (different user). Test (b) uses
 * {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, Object...)}
 * to start the server in the elevated helper and stop it again (no process variant).
 */
package org.appwork.utils.singleapp.tests;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.Application;
import org.appwork.utils.LogCallback;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.singleapp.AnotherInstanceRunningButFailedToConnectException;
import org.appwork.utils.singleapp.AnotherInstanceRunningException;
import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.ProtectedSingleAppInstance;
import org.appwork.utils.singleapp.ResponseSender;
import org.appwork.utils.singleapp.SingleAppInstance;

/**
 * Tests that ProtectedSingleAppInstance rejects requests from another user and (when server is elevated) from non-elevated processes.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class ProtectedSingleAppInstanceTest extends AWTest {
    private static final Charset UTF8 = Charset.forName("UTF-8");

    public static void main(String[] args) throws Exception {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("ProtectedSingleAppInstanceTest: skip (not Windows)");
            return;
        }
        logInfoAnyway("=== ProtectedSingleAppInstanceTest (Windows only) ===");
        testOtherSessionRejected();
        testElevatedServerRejectsNonElevatedClient();
    }

    /**
     * (a) Server runs in test process (normal user). Client runs as LocalSystem via runAsLocalSystem (different user). ProtectedSingleAppInstance must
     * reject the connection (socket address validator -> different user SID).
     */
    private void testOtherSessionRejected() throws Exception {
        logInfoAnyway("Test (a): requests from another user must be rejected");
        final File lockDir = Application.getTempResource("ProtectedSingleAppInstanceTest/session");
        lockDir.mkdirs();
        final String appID = "ProtectedSingleAppInstanceTestSession_" + System.currentTimeMillis();
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender sender, String[] message) {
            }
        };
        final ProtectedSingleAppInstance server = new ProtectedSingleAppInstance(appID, lockDir, listener);
        server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
        try {
            server.start(null, "");
            int port = readPortFromLockFile(lockDir, appID);
            logInfoAnyway("  Server listening on port " + port);
            logInfoAnyway("  runAsLocalSystem: ProtectedSingleAppInstanceTestClient.run(" + port + ")");
            String result = AdminExecuter.runAsLocalSystem(ProtectedSingleAppInstanceTestClient.class, "run", TypeRef.STRING, Integer.valueOf(port));
            assertTrue("REJECTED".equals(result), "Connection from other user must be REJECTED, got: " + result);
            logInfoAnyway("  OK: connection from LocalSystem (different user) rejected as expected");
        } finally {
            Thread exitThread = server.exit(true);
            if (exitThread != null) {
                exitThread.join(5000);
            }
        }
    }

    /**
     * (b) Server runs elevated (in helper via runAsAdmin). Client runs in test process (non-elevated). ProtectedSingleAppInstance must
     * reject the connection (socket address validator -> isProcessElevated false for client). Uses
     * {@link ProcessOptions#cancelCallback(CancelCallback)} to get a handle to stop the server in finally.
     */
    private void testElevatedServerRejectsNonElevatedClient() throws Exception {
        logInfoAnyway("Test (b): when server runs elevated, requests from non-elevated process must be rejected");
        if (WindowsUtils.isElevated()) {
            logInfoAnyway("  SKIP: test process is elevated; need non-elevated test to connect to elevated server");
            return;
        }
        final File lockDir = Application.getTempResource("ProtectedSingleAppInstanceTest/elevated");
        lockDir.mkdirs();
        final String appID = "ProtectedSingleAppInstanceTestElevated_" + System.currentTimeMillis();
        ProcessOptions options = ProcessOptions.builder().keepRunning(true).logCallback(new LogCallback() {
            @Override
            public void onStdOut(String line) {
                System.out.println("" + line);
            }

            @Override
            public void onStdErr(String line) {
            }
        }).build();
        try {
            logInfoAnyway("  runAsAdmin: start server in background (elevated) ...");
            Integer portObj;
            portObj = AdminExecuter.runAsAdmin(ProtectedSingleAppInstanceServerRunner.class, "startServerInBackgroundAndReturnPort", TypeRef.INT, options, lockDir.getAbsolutePath(), appID);
            assertTrue(options.isAlive());
            assertTrue(portObj != null && portObj.intValue() > 0, "Elevated server should return port; got: " + portObj);
            int port = portObj.intValue();
            logInfoAnyway("  Elevated server port: " + port);
            Thread.sleep(3000);
            final SingleAppInstance client = new ProtectedSingleAppInstance(appID, lockDir, null);
            try {
                client.start(null, "");
                assertTrue(false, "Non-elevated client should not succeed start() when server is elevated");
            } catch (AnotherInstanceRunningButFailedToConnectException e) {
                logInfoAnyway("  OK: non-elevated client was rejected (connection closed/failed, lock held by server)");
            } catch (AnotherInstanceRunningException e) {
                assertTrue(false, "We expected connection to be rejected (closed), not another instance running");
            } catch (SingleAppInstance.ErrorReadingResponseException e) {
                logInfoAnyway("  OK: non-elevated client was rejected (ErrorReadingResponseException)");
            } catch (IOException e) {
                logInfoAnyway("  OK: non-elevated client was rejected (IOException: " + e.getMessage() + ")");
            } finally {
                options.terminate();
            }
            client.exit();
        } finally {
        }
    }

    private static int readPortFromLockFile(File lockDir, String appID) throws Exception {
        File lockFile = new File(lockDir, appID + ".lock");
        long deadline = System.currentTimeMillis() + 10000;
        while (System.currentTimeMillis() < deadline) {
            if (lockFile.isFile() && lockFile.length() > 2) {
                FileInputStream fis = null;
                try {
                    fis = new FileInputStream(lockFile);
                    fis.getChannel().position(1);
                    byte[] buf = new byte[32];
                    int n = fis.read(buf);
                    if (n > 0) {
                        String s = new String(buf, 0, n, UTF8).trim().split("\r?\n")[0].trim();
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
