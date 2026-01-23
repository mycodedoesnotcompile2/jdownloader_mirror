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
package org.jdownloader.api.test;

import java.io.IOException;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HttpRequest;
import org.appwork.utils.net.httpserver.HttpResponse;
import org.jdownloader.api.DeprecatedAPIServer;

/**
 * Tests for DeprecatedAPIServer CORS (Cross-Origin Resource Sharing) functionality.
 *
 * <p>
 * This test class verifies that the DeprecatedAPIServer correctly blocks browser access:
 * </p>
 * <ul>
 * <li>Cross-origin requests (with Origin header) are blocked (403 Forbidden)</li>
 * <li>Direct server-to-server requests (without Origin header) are allowed (200 OK)</li>
 * <li>No CORS headers are present in responses (server is not accessible from browsers)</li>
 * <li>Localhost Origin requests are also blocked (security requirement)</li>
 * <li>Empty or invalid Origin headers are rejected</li>
 * </ul>
 *
 * <p>
 * <b>Security Requirement:</b> The DeprecatedAPIServer must NOT be accessible from browsers. This is enforced by:
 * </p>
 * <ul>
 * <li>Default CorsHandler with allowedOrigins=null (rejects all Origin requests)</li>
 * <li>All requests with Origin header return 403 Forbidden</li>
 * <li>No CORS headers are added to responses</li>
 * </ul>
 *
 * @author AppWork
 */
public class DeprecatedAPIServerCorsTest extends AWTest {

    private DeprecatedAPIServer deprecatedAPIServer;
    private RemoteAPI           remoteAPI;
    private int                 serverPort;
    private HttpClient         httpClient;
    private Throwable           lastServerException;
    private HttpRequest         lastRequest;
    private HttpResponse        lastResponse;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupDeprecatedAPIServer();
            this.testCrossOriginRequestBlocked();
            this.testDirectServerRequestAllowed();
            this.testLocalhostOriginBlocked();
            this.testEmptyOriginHeaderBlocked();
            this.testNoCorsHeadersInResponse();
            this.testMultipleOriginValuesBlocked();
        } finally {
            this.teardownDeprecatedAPIServer();
        }
    }

    /**
     * Server Setup: Creates and starts a DeprecatedAPIServer with Dummy API
     */
    protected void setupDeprecatedAPIServer() throws IOException, ParseException {
        LogV3.info("Starting DeprecatedAPIServer Setup...");

        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Create DeprecatedAPIServer on a free port (0 = automatic)
        // DeprecatedAPIServer automatically creates a CorsHandler with default settings (allowedOrigins=null)
        this.deprecatedAPIServer = new DeprecatedAPIServer(0);
        this.deprecatedAPIServer.setLocalhostOnly(true);

        // Register RemoteAPI as request handler
        this.deprecatedAPIServer.registerRequestHandler(this.remoteAPI);

        // Start server
        this.deprecatedAPIServer.start();
        this.serverPort = this.deprecatedAPIServer.getActualPort();

        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout(5000);
        this.httpClient.setReadTimeout(30000);
        // Set default mandatory header for all requests (x-appwork header)
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");

        LogV3.info("DeprecatedAPIServer started on port: " + this.serverPort);
    }

    /**
     * Server Teardown: Stops the server
     */
    protected void teardownDeprecatedAPIServer() {
        if (this.deprecatedAPIServer != null) {
            try {
                this.deprecatedAPIServer.shutdown();
                LogV3.info("DeprecatedAPIServer stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Helper method to assert that no CORS headers are present in the response
     */
    private void assertNoCorsHeaders(final RequestContext context, final String messagePrefix) throws Exception {
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);
        final String exposeHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS);

        assertTrue(allowOrigin == null, messagePrefix + ": Access-Control-Allow-Origin header should not be present, but was: " + allowOrigin);
        assertTrue(allowMethods == null, messagePrefix + ": Access-Control-Allow-Methods header should not be present, but was: " + allowMethods);
        assertTrue(allowHeaders == null, messagePrefix + ": Access-Control-Allow-Headers header should not be present, but was: " + allowHeaders);
        assertTrue(maxAge == null, messagePrefix + ": Access-Control-Max-Age header should not be present, but was: " + maxAge);
        assertTrue(exposeHeaders == null, messagePrefix + ": Access-Control-Expose-Headers header should not be present, but was: " + exposeHeaders);
    }

    /**
     * Test 1: Cross-origin request from foreign domain should be blocked (403 Forbidden)
     *
     * <p>
     * This test verifies that browser requests (with Origin header) are rejected, ensuring the server is not accessible from browsers.
     * </p>
     */
    private void testCrossOriginRequestBlocked() throws Exception {
        LogV3.info("Test 1: Cross-origin request from foreign domain blocked");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(url));
        final int responseCode = context.getCode();

        // Cross-origin requests should be rejected with 403 Forbidden
        assertTrue(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Cross-Origin request should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode);

        // Check that no CORS headers are present in the response (request was blocked)
        this.assertNoCorsHeaders(context, "Cross-origin request blocked");

        LogV3.info("Test 1 passed: Cross-origin request blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 2: Direct server-to-server request (no Origin) should be allowed (200 OK)
     *
     * <p>
     * This test verifies that server-to-server communication (without Origin header) works correctly. This is the intended use case for
     * DeprecatedAPIServer.
     * </p>
     */
    private void testDirectServerRequestAllowed() throws Exception {
        LogV3.info("Test 2: Direct server-to-server request (no Origin) allowed");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Don't set Origin header - should be allowed (server-to-server communication)
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Direct server-to-server request (no Origin) should return 200, was: " + responseCode);

        // No CORS headers should be present (no Origin = no CORS)
        this.assertNoCorsHeaders(context, "Direct server request");

        LogV3.info("Test 2 passed: Direct server-to-server request successful: " + responseCode);
    }

    /**
     * Test 3: Localhost Origin should be blocked (security requirement)
     *
     * <p>
     * Even localhost Origin requests must be blocked to prevent browser access. Only requests without Origin header (server-to-server) are
     * allowed.
     * </p>
     */
    private void testLocalhostOriginBlocked() throws Exception {
        LogV3.info("Test 3: Localhost Origin blocked");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://localhost:" + this.serverPort).setUrl(url));
        final int responseCode = context.getCode();
        assertTrue(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Localhost Origin should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode);
        this.assertNoCorsHeaders(context, "Localhost Origin blocked");
        LogV3.info("Test 3 passed: Localhost Origin blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 4: Empty Origin header should be blocked
     */
    private void testEmptyOriginHeaderBlocked() throws Exception {
        LogV3.info("Test 4: Empty Origin header blocked");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "").setUrl(url));
        final int responseCode = context.getCode();
        assertTrue(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Empty Origin header should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode);
        this.assertNoCorsHeaders(context, "Empty Origin header blocked");
        LogV3.info("Test 4 passed: Empty Origin header blocked with " + responseCode);
    }

    /**
     * Test 5: Verify no CORS headers are present in any response
     *
     * <p>
     * This test ensures that even if a request somehow succeeds, no CORS headers are added, confirming the server is not configured for
     * browser access.
     * </p>
     */
    private void testNoCorsHeadersInResponse() throws Exception {
        LogV3.info("Test 5: No CORS headers in response");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Request without Origin (should succeed)
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should return 200, was: " + responseCode);

        // Verify no CORS headers are present
        this.assertNoCorsHeaders(context, "No CORS headers in response");

        LogV3.info("Test 5 passed: No CORS headers present in response");
    }

    /**
     * Test 6: Multiple different Origin values should all be blocked
     *
     * <p>
     * This test verifies that various Origin values (including common browser origins) are all rejected.
     * </p>
     */
    private void testMultipleOriginValuesBlocked() throws Exception {
        LogV3.info("Test 6: Multiple Origin values blocked");
        final String[] origins = { "https://example.com", "https://evil.com", "http://localhost", "http://127.0.0.1", "null", "   " };

        for (final String origin : origins) {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
            final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));
            final int responseCode = context.getCode();
            assertTrue(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Origin '" + origin + "' should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode);
            this.assertNoCorsHeaders(context, "Origin '" + origin + "' blocked");
            LogV3.info("  Origin '" + origin + "' correctly blocked with " + responseCode);
        }

        LogV3.info("Test 6 passed: All Origin values blocked");
    }
}
