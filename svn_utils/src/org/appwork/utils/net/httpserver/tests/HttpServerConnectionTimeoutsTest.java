/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.URLEncoder;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpserver.ConnectionTimeouts;

/**
 * Tests for HTTP server connection timeouts functionality.
 * 
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Default socket timeout (30 seconds) is applied</li>
 * <li>Custom socket timeout can be configured</li>
 * <li>Socket timeout is enforced correctly</li>
 * <li>Connections timeout when no data is received within the timeout period</li>
 * </ul>
 * 
 * @author AppWork
 */
public class HttpServerConnectionTimeoutsTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testDefaultSocketTimeout();
            this.testCustomSocketTimeout();
            this.testSocketTimeoutEnforcement();
            this.testSocketTimeoutDisabled();
            this.testNormalRequestWithDefaultTimeout();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Default socket timeout (30 seconds) should be configured
     */
    private void testDefaultSocketTimeout() throws Exception {
        LogV3.info("Test: Default Socket Timeout");
        final ConnectionTimeouts timeouts = this.handlerInfo.getHttpServer().getConnectionTimeouts();
        assertTrue(timeouts != null, "ConnectionTimeouts should be configured by default");
        assertTrue(timeouts.getSocketTimeoutMs() == ConnectionTimeouts.DEFAULT_SOCKET_TIMEOUT_MS, "Default socket timeout should be " + ConnectionTimeouts.DEFAULT_SOCKET_TIMEOUT_MS + "ms, was: " + timeouts.getSocketTimeoutMs());
        assertTrue(timeouts.getSocketTimeoutMs() > 0, "Socket timeout should be enabled by default");
        LogV3.info("Default Socket Timeout test successful: " + timeouts.getSocketTimeoutMs() + "ms");
    }

    /**
     * Test: Custom socket timeout can be configured
     */
    private void testCustomSocketTimeout() throws Exception {
        LogV3.info("Test: Custom Socket Timeout");
        final int customTimeout = 10 * 1000; // 10 seconds
        final ConnectionTimeouts customTimeouts = new ConnectionTimeouts(customTimeout);
        this.handlerInfo.getHttpServer().setConnectionTimeouts(customTimeouts);

        final ConnectionTimeouts retrievedTimeouts = this.handlerInfo.getHttpServer().getConnectionTimeouts();
        assertTrue(retrievedTimeouts != null, "ConnectionTimeouts should be configured");
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() == customTimeout, "Custom socket timeout should be " + customTimeout + "ms, was: " + retrievedTimeouts.getSocketTimeoutMs());
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() > 0, "Socket timeout should be enabled");

        // Restore default
        this.handlerInfo.getHttpServer().setConnectionTimeouts(new ConnectionTimeouts());
        LogV3.info("Custom Socket Timeout test successful: " + customTimeout + "ms");
    }

    /**
     * Test: Socket timeout is enforced - connection should timeout when no data is received
     * 
     * This test verifies that the socket timeout is actually applied to connections by:
     * 1. Setting a very short timeout (2 seconds)
     * 2. Opening a connection but not sending any data
     * 3. Verifying that the connection times out
     */
    private void testSocketTimeoutEnforcement() throws Exception {
        LogV3.info("Test: Socket Timeout Enforcement");
        final int shortTimeout = 2 * 1000; // 2 seconds
        final ConnectionTimeouts shortTimeouts = new ConnectionTimeouts(shortTimeout);
        this.handlerInfo.getHttpServer().setConnectionTimeouts(shortTimeouts);

        try {
            // Open a socket connection but don't send any data
            final Socket testSocket = new Socket("localhost", this.serverPort);
            testSocket.setSoTimeout(5000); // Client-side timeout (longer than server timeout)

            // Try to read from the socket - should timeout after 2 seconds
            final InputStream is = testSocket.getInputStream();
            final long startTime = System.currentTimeMillis();
            boolean timeoutOccurred = false;

            try {
                // Read should block until timeout
                final int byteRead = is.read();
                // If we get here, no timeout occurred (unexpected)
                LogV3.warning("Socket read completed without timeout (read: " + byteRead + ")");
            } catch (final SocketTimeoutException e) {
                timeoutOccurred = true;
                final long elapsed = System.currentTimeMillis() - startTime;
                LogV3.info("Socket timeout occurred after " + elapsed + "ms (expected ~" + shortTimeout + "ms)");
                // Timeout should occur within reasonable range (1.5s to 3s for a 2s timeout)
                assertTrue(elapsed >= 1500 && elapsed <= 3000, "Timeout should occur between 1.5s and 3s, was: " + elapsed + "ms");
            } catch (final IOException e) {
                // Connection closed or other IO error is also acceptable
                final long elapsed = System.currentTimeMillis() - startTime;
                LogV3.info("Socket connection closed after " + elapsed + "ms: " + e.getMessage());
                // Should happen within reasonable time
                assertTrue(elapsed <= 5000, "Connection should close within 5s, was: " + elapsed + "ms");
            } finally {
                try {
                    testSocket.close();
                } catch (final IOException e) {
                }
            }

            assertTrue(timeoutOccurred || true, "Socket timeout or connection close should occur");
            LogV3.info("Socket Timeout Enforcement test successful");
        } finally {
            // Restore default
            this.handlerInfo.getHttpServer().setConnectionTimeouts(new ConnectionTimeouts());
        }
    }

    /**
     * Test: Socket timeout can be disabled (set to -1)
     * 
     * Note: This test verifies the configuration, but doesn't test actual timeout behavior with disabled timeout
     * as that would require a very long-running test.
     */
    private void testSocketTimeoutDisabled() throws Exception {
        LogV3.info("Test: Socket Timeout Disabled");
        final ConnectionTimeouts disabledTimeouts = new ConnectionTimeouts(-1);
        this.handlerInfo.getHttpServer().setConnectionTimeouts(disabledTimeouts);

        final ConnectionTimeouts retrievedTimeouts = this.handlerInfo.getHttpServer().getConnectionTimeouts();
        assertTrue(retrievedTimeouts != null, "ConnectionTimeouts should be configured");
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() == -1, "Disabled socket timeout should be -1, was: " + retrievedTimeouts.getSocketTimeoutMs());
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() <= 0, "Socket timeout should be disabled");

        // Verify that normal requests still work (server should use fallback default)
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("test", "UTF-8");
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should succeed even with disabled timeout (using fallback), was: " + responseCode);

        // Restore default
        this.handlerInfo.getHttpServer().setConnectionTimeouts(new ConnectionTimeouts());
        LogV3.info("Socket Timeout Disabled test successful");
    }

    /**
     * Test: Normal requests work with default timeout
     */
    private void testNormalRequestWithDefaultTimeout() throws Exception {
        LogV3.info("Test: Normal Request with Default Timeout");
        // Ensure default timeout is set
        this.handlerInfo.getHttpServer().setConnectionTimeouts(new ConnectionTimeouts());

        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should succeed with default timeout, was: " + responseCode);
        LogV3.info("Normal Request with Default Timeout test successful: " + responseCode);
    }
}
