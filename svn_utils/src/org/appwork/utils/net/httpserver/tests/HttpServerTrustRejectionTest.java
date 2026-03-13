/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver.tests;

import java.util.concurrent.TimeUnit;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.os.CrossSystem;

/**
 * Tests that HttpServer (localhost-only, Windows) rejects connections when the server's socket address validator
 * rejects the connection: request from different user (e.g. LocalSystem), and request from non-elevated client when
 * server runs elevated. Runs only on Windows; skips on other OS.
 */
public class HttpServerTrustRejectionTest extends AWTest {
    /**
     * @author thomas
     * @date 06.03.2026
     *
     */
    public static final class ShouldWork implements ElevatedTestTask {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        /**
         *
         */
        private final int         port;

        /**
         * @param port
         */
        public ShouldWork(int port) {
            this.port = port;
        }

        @Override
        public Boolean run() throws Exception {
            HttpClient httpClient = new HttpClient();
            httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(10));
            httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(8));
            LogV3.info("" + httpClient.get("http://127.0.0.1:" + port + "/"));
            return true;
        }
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("HttpServerTrustRejectionTest: skip (not Windows)");
            return;
        }
        logInfoAnyway("=== HttpServer trust rejection (different user / elevation) ===");
        testHttpServerRejectsLocalSystemSession();
        logInfoAnyway("=== HttpServer elevated rejects non-elevated client ===");
        testHttpServerElevatedRejectsNonElevatedClient();
    }

    /**
     * HttpServer bound to localhost must reject HTTP requests from a client running as a different user (LocalSystem). The client is run
     * via runAsLocalSystem; the server must close the connection without response (socket address validator returns false). Asserts the
     * client exit code is 0 (rejected).
     */
    private void testHttpServerRejectsLocalSystemSession() throws Exception {
        logInfoAnyway("Test: HttpServer (localhost) rejects request from different user (LocalSystem)");
        HttpServer server = null;
        try {
            server = new HttpServer(0);
            server.setLocalhostOnly(true);
            server.start();
            final int port = server.getActualPort();
            logInfoAnyway("  runAsLocalSystem: HttpServerTrustTestClient.run(" + port + ")");
            Integer result = AdminExecuter.runAsLocalSystem(HttpServerTrustTestClient.class, "run", TypeRef.INT, Integer.valueOf(port));
            assertTrue(result != null && result.intValue() == 0, "HttpServer must reject different-user request (run() 0=rejected, 1=accepted); got: " + result);
            logInfoAnyway("  OK: request from LocalSystem (different user) was rejected as expected");
        } finally {
            if (server != null) {
                try {
                    server.shutdown();
                } catch (Throwable t) {
                }
            }
        }
    }

    /**
     * HttpServer running elevated must reject HTTP requests from a non-elevated client. The server is started in the elevated helper; this
     * test process (non-elevated) connects and sends a GET. The server must close the connection without response. Asserts the connection
     * was rejected. The server is stopped via ProcessOptions cancel callback in finally.
     */
    private void testHttpServerElevatedRejectsNonElevatedClient() throws Exception {
        logInfoAnyway("Test: HttpServer (elevated) rejects request from non-elevated client");
        ProcessOptions options = ProcessOptions.builder().keepRunning(true).build();
        try {
            Integer portObj = AdminExecuter.runAsAdmin(HttpServerTrustTestServer.class, "startServerInBackgroundAndReturnPort", TypeRef.INT, options);
            assertTrue(portObj != null && portObj.intValue() > 0, "Elevated server should return port; got: " + portObj);
            int port = portObj.intValue();
            HttpClient httpClient = new HttpClient();
            httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(10));
            httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(8));
            boolean rejected = false;
            try {
                httpClient.get("http://127.0.0.1:" + port + "/");
            } catch (HttpClientException e) {
                rejected = true;
            } catch (java.io.IOException e) {
                rejected = true;
            }
            assertTrue(rejected, "HttpServer (elevated) must reject non-elevated client (connection closed without response)");
            assertTrue(AdminExecuter.runAsAdmin(new ShouldWork(port), TypeRef.BOOLEAN));
            logInfoAnyway("  OK: non-elevated client was rejected as expected");
        } finally {
            options.terminate();
        }
    }

    public static void main(String[] args) {
        run();
    }
}
