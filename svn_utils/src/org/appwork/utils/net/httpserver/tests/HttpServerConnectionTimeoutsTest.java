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
        final ConnectionTimeouts timeouts = httpServer.getConnectionTimeouts();
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
        httpServer.setConnectionTimeouts(customTimeouts);

        final ConnectionTimeouts retrievedTimeouts = httpServer.getConnectionTimeouts();
        assertTrue(retrievedTimeouts != null, "ConnectionTimeouts should be configured");
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() == customTimeout, "Custom socket timeout should be " + customTimeout + "ms, was: " + retrievedTimeouts.getSocketTimeoutMs());
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() > 0, "Socket timeout should be enabled");

        // Restore default
        httpServer.setConnectionTimeouts(new ConnectionTimeouts());
        LogV3.info("Custom Socket Timeout test successful: " + customTimeout + "ms");
    }

    /**
     * Test: Socket timeout is enforced - connection should timeout when no data is received
     *
     * This test verifies that the socket timeout is actually applied to connections by: 1. Setting a very short timeout (2 seconds) 2.
     * Opening a connection but not sending any data 3. Verifying that the server closes the connection after the timeout
     *
     * Note: The total time is just the socket timeout (~2 seconds). Draining is skipped for SocketTimeoutException because the socket is
     * already dead and no data will arrive.
     */
    private void testSocketTimeoutEnforcement() throws Exception {
        LogV3.info("Test: Socket Timeout Enforcement");
        final int shortTimeout = 2 * 1000; // 2 seconds
        final ConnectionTimeouts shortTimeouts = new ConnectionTimeouts(shortTimeout);
        httpServer.setConnectionTimeouts(shortTimeouts);

        try {
            // Open a socket connection but don't send any data
            final Socket testSocket = new Socket("localhost", this.serverPort);
            // Don't set client timeout - we want to test the server timeout

            // Try to read from the socket - server should close connection after ~2 seconds
            // Note: Draining is skipped for SocketTimeoutException, so no additional drain timeout
            final InputStream is = testSocket.getInputStream();
            final long startTime = System.currentTimeMillis();
            lastServerException = null;
            try {
                // Read should block until server closes connection
                // Expected: Server timeout after 2s, no draining (because socket is dead), immediate connection close
                final int byteRead = is.read();
                final long elapsed = System.currentTimeMillis() - startTime;

                assertTrue(lastServerException instanceof SocketTimeoutException, "We expect a read timeout while reading the header");
                // -1 means connection closed by server (this is what should happen)
                assertTrue(byteRead == -1, "Expected EOF (read() == -1) after server timeout, got: " + byteRead);
                LogV3.info("Server closed connection after " + elapsed + "ms (expected ~" + shortTimeout + "ms = socket timeout, no drain)");
                // Should happen within reasonable range (1.5s to 3s for a 2s timeout)
                assertTrue(elapsed >= 1500 && elapsed <= 3000, "Connection close should occur between 1.5s and 3s, was: " + elapsed + "ms");
            } catch (final SocketTimeoutException e) {
                // This should NOT happen since we don't set client timeout
                final long elapsed = System.currentTimeMillis() - startTime;
                throw new AssertionError("Unexpected client-side timeout occurred after " + elapsed + "ms: " + e.getMessage(), e);
            } catch (final IOException e) {
                // This should also not happen in the normal case, but could happen depending on OS/timing
                final long elapsed = System.currentTimeMillis() - startTime;
                LogV3.warning("IOException during read (instead of EOF): " + e.getMessage() + " after " + elapsed + "ms");
                // Still verify timing
                assertTrue(elapsed >= 1500 && elapsed <= 3000, "Connection should close between 1.5s and 3s, was: " + elapsed + "ms");
            } finally {
                try {
                    testSocket.close();
                } catch (final IOException e) {
                }
            }

            LogV3.info("Socket Timeout Enforcement test successful");
        } finally {
            // Restore default
            httpServer.setConnectionTimeouts(new ConnectionTimeouts());
        }
    }

    /**
     * Test: Socket timeout can be disabled (set to -1)
     *
     * Note: This test verifies the configuration, but doesn't test actual timeout behavior with disabled timeout as that would require a
     * very long-running test.
     */
    private void testSocketTimeoutDisabled() throws Exception {
        LogV3.info("Test: Socket Timeout Disabled");
        final ConnectionTimeouts disabledTimeouts = new ConnectionTimeouts(-1);
        httpServer.setConnectionTimeouts(disabledTimeouts);

        final ConnectionTimeouts retrievedTimeouts = httpServer.getConnectionTimeouts();
        assertTrue(retrievedTimeouts != null, "ConnectionTimeouts should be configured");
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() == -1, "Disabled socket timeout should be -1, was: " + retrievedTimeouts.getSocketTimeoutMs());
        assertTrue(retrievedTimeouts.getSocketTimeoutMs() <= 0, "Socket timeout should be disabled");

        // Verify that normal requests still work (server should use fallback default)
        lastServerException = null;
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("test", "UTF-8");
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should succeed even with disabled timeout (using fallback), was: " + responseCode);
        // Server-side: No exception should occur
        assertTrue(lastServerException == null, "Server-side: No exception expected for successful request, but got: " + lastServerException);

        // Restore default
        httpServer.setConnectionTimeouts(new ConnectionTimeouts());
        LogV3.info("Socket Timeout Disabled test successful");
    }

    /**
     * Test: Normal requests work with default timeout
     */
    private void testNormalRequestWithDefaultTimeout() throws Exception {
        LogV3.info("Test: Normal Request with Default Timeout");
        // Ensure default timeout is set
        httpServer.setConnectionTimeouts(new ConnectionTimeouts());

        lastServerException = null;
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should succeed with default timeout, was: " + responseCode);
        // Server-side: No exception should occur
        assertTrue(lastServerException == null, "Server-side: No exception expected for successful request, but got: " + lastServerException);
        LogV3.info("Normal Request with Default Timeout test successful: " + responseCode);
    }
}
