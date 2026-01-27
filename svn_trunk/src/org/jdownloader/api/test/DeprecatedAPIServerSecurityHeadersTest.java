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
import org.appwork.remoteapi.SessionRemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.storage.config.JsonConfig;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HttpHandlerInfo;
import org.appwork.utils.net.httpserver.HttpServer;
import org.jdownloader.api.DeprecatedAPIHttpServerController;
import org.jdownloader.api.RemoteAPIConfig;
import org.jdownloader.api.RemoteAPISession;
import org.jdownloader.api.RemoteAPISessionControllerImp;

/**
 * Tests for DeprecatedAPIServer security headers configuration.
 *
 * <p>
 * This test class verifies that the DeprecatedAPIServer correctly applies security settings from RemoteAPIConfig:
 * </p>
 * <ul>
 * <li>CORS (Cross-Origin Resource Sharing) settings from getLocalAPIServerHeaderAccessControllAllowOrigin()</li>
 * <li>Content-Security-Policy from getLocalAPIServerHeaderContentSecurityPolicy()</li>
 * <li>X-Frame-Options from getLocalAPIServerHeaderXFrameOptions()</li>
 * <li>Referrer-Policy from getLocalAPIServerHeaderReferrerPolicy()</li>
 * <li>X-Content-Type-Options (should be nosniff by default)</li>
 * </ul>
 *
 * <p>
 * The test initializes the server exactly like in JDownloader:
 * </p>
 * <ul>
 * <li>Uses DeprecatedAPIHttpServerController.getInstance()</li>
 * <li>Creates RemoteAPISessionControllerImp</li>
 * <li>Creates RemoteAPI and registers it with the session controller</li>
 * <li>Registers the session controller with the server controller</li>
 * </ul>
 *
 * @author AppWork
 */
public class DeprecatedAPIServerSecurityHeadersTest extends AWTest {
    private HttpHandlerInfo                    serverHandlerInfo;
    private RemoteAPISessionControllerImp      sessionController;
    private SessionRemoteAPI<RemoteAPISession> remoteAPI;
    private int                                serverPort;
    private HttpClient                         httpClient;
    private RemoteAPIConfig                    config;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupDeprecatedAPIServer();
            this.testSecurityHeadersFromConfig();
            this.testCorsConfiguration();
            this.testContentSecurityPolicy();
            this.testXFrameOptions();
            this.testReferrerPolicy();
            this.testXContentTypeOptions();
        } finally {
            this.teardownDeprecatedAPIServer();
        }
    }

    /**
     * Server Setup: Creates and starts a DeprecatedAPIServer exactly like in JDownloader
     */
    protected void setupDeprecatedAPIServer() throws IOException, ParseException {
        LogV3.info("Starting DeprecatedAPIServer Setup (like in JDownloader)...");
        // Get configuration
        this.config = JsonConfig.create(RemoteAPIConfig.class);
        // Create SessionRemoteAPI and register Dummy API (like in RemoteAPIController)
        this.remoteAPI = new SessionRemoteAPI<RemoteAPISession>();
        this.remoteAPI.register(new DummyTestAPIImpl());
        // Create RemoteAPISessionControllerImp (like in RemoteAPIController)
        this.sessionController = new RemoteAPISessionControllerImp();
        // Register RemoteAPI with session controller (like in RemoteAPIController)
        this.sessionController.registerSessionRequestHandler(this.remoteAPI);
        this.remoteAPI.register(this.sessionController);
        // Register with DeprecatedAPIHttpServerController (like in RemoteAPIController)
        // Use port 0 for automatic port assignment
        this.serverHandlerInfo = DeprecatedAPIHttpServerController.getInstance().registerRequestHandler(0, true, this.sessionController);
        // Get the actual server and port
        HttpServer server = this.serverHandlerInfo.getHttpServer();
        this.serverPort = server.getActualPort();
        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout(5000);
        this.httpClient.setReadTimeout(30000);
        LogV3.info("DeprecatedAPIServer started on port: " + this.serverPort);
    }

    /**
     * Server Teardown: Stops the server
     */
    protected void teardownDeprecatedAPIServer() {
        if (this.serverHandlerInfo != null) {
            try {
                DeprecatedAPIHttpServerController.getInstance().unregisterRequestHandler(this.serverHandlerInfo);
                LogV3.info("DeprecatedAPIServer stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Test: Verify that security headers match the configuration from RemoteAPIConfig
     */
    private void testSecurityHeadersFromConfig() throws Exception {
        LogV3.info("Test: Security headers match RemoteAPIConfig");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Request should return 200, was: " + responseCode);
        // Get actual header values from response
        final String csp = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
        final String xFrameOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
        final String referrerPolicy = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
        final String xContentTypeOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
        // Verify Content-Security-Policy matches config
        final String expectedCsp = this.config.getLocalAPIServerHeaderContentSecurityPolicy();
        if (expectedCsp != null && !expectedCsp.isEmpty()) {
            assertTrue(csp != null && csp.equals(expectedCsp), "Content-Security-Policy should match config. Expected: " + expectedCsp + ", Actual: " + csp);
        }
        // Verify X-Frame-Options matches config
        final String expectedXFrameOptions = this.config.getLocalAPIServerHeaderXFrameOptions();
        if (expectedXFrameOptions != null && !expectedXFrameOptions.isEmpty()) {
            assertTrue(xFrameOptions != null && xFrameOptions.equals(expectedXFrameOptions), "X-Frame-Options should match config. Expected: " + expectedXFrameOptions + ", Actual: " + xFrameOptions);
        }
        // Verify Referrer-Policy matches config
        final String expectedReferrerPolicy = this.config.getLocalAPIServerHeaderReferrerPolicy();
        if (expectedReferrerPolicy != null && !expectedReferrerPolicy.isEmpty()) {
            assertTrue(referrerPolicy != null && referrerPolicy.equals(expectedReferrerPolicy), "Referrer-Policy should match config. Expected: " + expectedReferrerPolicy + ", Actual: " + referrerPolicy);
        }
        // Verify X-Content-Type-Options is nosniff (default)
        assertTrue(xContentTypeOptions != null && xContentTypeOptions.equals("nosniff"), "X-Content-Type-Options should be 'nosniff'. Actual: " + xContentTypeOptions);
        LogV3.info("Test passed: Security headers match RemoteAPIConfig");
    }

    /**
     * Test: Verify CORS configuration from RemoteAPIConfig
     */
    private void testCorsConfiguration() throws Exception {
        LogV3.info("Test: CORS configuration from RemoteAPIConfig");
        final String allowedOrigin = this.config.getLocalAPIServerHeaderAccessControllAllowOrigin();
        if (allowedOrigin == null || allowedOrigin.isEmpty()) {
            // No CORS origin configured - test that cross-origin requests are blocked
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
            final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(url));
            final int responseCode = context.getCode();
            // Cross-origin requests should be blocked (403 Forbidden)
            assertTrue(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Cross-origin request should be blocked (403) when no CORS origin is configured. Response code: " + responseCode);
            LogV3.info("Test passed: CORS correctly blocks requests when no origin is configured");
        } else {
            // CORS origin configured - test that it's allowed
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
            final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", allowedOrigin).addHeader("Access-Control-Request-Method", "GET").setUrl(url));
            final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
            if ("*".equals(allowedOrigin)) {
                assertTrue("*".equals(allowOrigin), "Access-Control-Allow-Origin should be '*' when config is '*'. Actual: " + allowOrigin);
            } else {
                assertTrue(allowedOrigin.equals(allowOrigin), "Access-Control-Allow-Origin should match config. Expected: " + allowedOrigin + ", Actual: " + allowOrigin);
            }
            LogV3.info("Test passed: CORS correctly allows configured origin: " + allowedOrigin);
        }
    }

    /**
     * Test: Verify Content-Security-Policy header
     */
    private void testContentSecurityPolicy() throws Exception {
        LogV3.info("Test: Content-Security-Policy header");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final RequestContext context = this.httpClient.get(url);
        final String csp = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
        final String expectedCsp = this.config.getLocalAPIServerHeaderContentSecurityPolicy();
        if (expectedCsp != null && !expectedCsp.isEmpty()) {
            assertTrue(csp != null, "Content-Security-Policy header should be present when configured");
            assertTrue(csp.equals(expectedCsp), "Content-Security-Policy should match config. Expected: " + expectedCsp + ", Actual: " + csp);
        } else {
            // CSP may or may not be present when not configured - depends on implementation
            LogV3.info("Content-Security-Policy not configured in RemoteAPIConfig (empty or null)");
        }
        LogV3.info("Test passed: Content-Security-Policy header verified");
    }

    /**
     * Test: Verify X-Frame-Options header
     */
    private void testXFrameOptions() throws Exception {
        LogV3.info("Test: X-Frame-Options header");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final RequestContext context = this.httpClient.get(url);
        final String xFrameOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
        final String expectedXFrameOptions = this.config.getLocalAPIServerHeaderXFrameOptions();
        if (expectedXFrameOptions != null && !expectedXFrameOptions.isEmpty()) {
            assertTrue(xFrameOptions != null, "X-Frame-Options header should be present when configured");
            assertTrue(xFrameOptions.equals(expectedXFrameOptions), "X-Frame-Options should match config. Expected: " + expectedXFrameOptions + ", Actual: " + xFrameOptions);
        } else {
            // X-Frame-Options may have a default value even when not explicitly configured
            LogV3.info("X-Frame-Options config is empty or null, but header may still be present with default value");
        }
        LogV3.info("Test passed: X-Frame-Options header verified");
    }

    /**
     * Test: Verify Referrer-Policy header
     */
    private void testReferrerPolicy() throws Exception {
        LogV3.info("Test: Referrer-Policy header");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final RequestContext context = this.httpClient.get(url);
        final String referrerPolicy = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
        final String expectedReferrerPolicy = this.config.getLocalAPIServerHeaderReferrerPolicy();
        if (expectedReferrerPolicy != null && !expectedReferrerPolicy.isEmpty()) {
            assertTrue(referrerPolicy != null, "Referrer-Policy header should be present when configured");
            assertTrue(referrerPolicy.equals(expectedReferrerPolicy), "Referrer-Policy should match config. Expected: " + expectedReferrerPolicy + ", Actual: " + referrerPolicy);
        } else {
            // Referrer-Policy may have a default value even when not explicitly configured
            LogV3.info("Referrer-Policy config is empty or null, but header may still be present with default value");
        }
        LogV3.info("Test passed: Referrer-Policy header verified");
    }

    /**
     * Test: Verify X-Content-Type-Options header (should always be nosniff)
     */
    private void testXContentTypeOptions() throws Exception {
        LogV3.info("Test: X-Content-Type-Options header");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final RequestContext context = this.httpClient.get(url);
        final String xContentTypeOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
        // X-Content-Type-Options should always be "nosniff" (default security setting)
        assertTrue(xContentTypeOptions != null, "X-Content-Type-Options header should be present");
        assertTrue("nosniff".equals(xContentTypeOptions), "X-Content-Type-Options should be 'nosniff'. Actual: " + xContentTypeOptions);
        LogV3.info("Test passed: X-Content-Type-Options is 'nosniff'");
    }
}
