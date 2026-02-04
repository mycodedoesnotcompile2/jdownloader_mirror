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

import java.util.Arrays;
import java.util.EnumSet;
import java.util.concurrent.TimeUnit;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.ForbiddenOriginException;
import org.appwork.utils.net.httpserver.OriginRule;

/**
 * CORS preflight request tests for HTTP server.
 *
 * <p>
 * This test class covers all aspects of CORS preflight requests (OPTIONS method), including:
 * </p>
 * <ul>
 * <li>Basic preflight request handling</li>
 * <li>Forbidden origins and methods</li>
 * <li>Various HTTP methods and headers</li>
 * <li>Wildcard origins</li>
 * <li>Path-aware CORS preflight requests</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerCorsPreflightTest extends HttpServerTestBase {
    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        java.util.Set<RequestMethod> previousMethods = null;
        try {
            this.setupServer();
            // Allow OPTIONS for CORS preflight tests
            previousMethods = this.allowHttpMethods(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST);
            this.testCorsPreflightRequest();
            this.testCorsPreflightRequestBlockedForbiddenOrigin();
            this.testCorsPreflightRequestBlockedForbiddenMethod();
            this.testCorsPreflightRequestWithVariousMethods();
            this.testCorsPreflightRequestWithVariousHeaders();
            this.testCorsPreflightRequestWithoutOrigin();
            this.testCorsPreflightRequestWithWildcardOrigin();
            this.testCorsPreflightRequestPathAware();
            this.testCorsPreflightRequestPathAwareBlocked();
        } finally {
            if (previousMethods != null) {
                this.restoreHttpMethods(previousMethods);
            }
            this.teardownServer();
        }
    }

    /**
     * Test 11: CORS preflight request (OPTIONS) should work correctly
     */
    private void testCorsPreflightRequest() throws Exception {
        LogV3.info("Test 11: CORS preflight request (OPTIONS)");
        // Create and configure CORS handler for this test
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com"), new OriginRule("https://another.example")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type, Authorization").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "OPTIONS preflight request should return 200/204, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed origin, but got: " + this.lastServerException, context);
        // Check all CORS headers are present and have correct values
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);
        // Verify Access-Control-Allow-Origin matches the requesting origin
        assertEquals(origin, allowOrigin, "Access-Control-Allow-Origin should match Origin");
        // Verify Access-Control-Allow-Methods contains all configured methods
        this.assertTrueWithContext(allowMethods != null, "Access-Control-Allow-Methods should be present in preflight response", context);
        this.assertTrueWithContext(allowMethods.contains("OPTIONS") && allowMethods.contains("GET") && allowMethods.contains("POST"), "Access-Control-Allow-Methods should contain OPTIONS, GET, POST, was: " + allowMethods, context);
        // Verify Access-Control-Allow-Headers echoes the requested headers
        assertEquals("Content-Type, Authorization", allowHeaders, "Access-Control-Allow-Headers should echo request headers");
        // Verify Access-Control-Max-Age matches configured value (30 seconds = 30000 milliseconds)
        assertEquals("30", maxAge, "Access-Control-Max-Age should be 30 seconds, was: " + maxAge);
        LogV3.info("Test 11 passed: CORS preflight request successful");
    }

    /**
     * Test 11a: Preflight request with forbidden origin should be blocked
     */
    private void testCorsPreflightRequestBlockedForbiddenOrigin() throws Exception {
        LogV3.info("Test 11a: Preflight request with forbidden origin should be blocked");
        // Create and configure CORS handler for this test
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com"), new OriginRule("https://another.example")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String forbiddenOrigin = "https://boeseseite.com";
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", forbiddenOrigin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Preflight request with forbidden origin should return 403 Forbidden, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // CORS headers should not be present
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin == null, "Access-Control-Allow-Origin should not be present for forbidden origin", context);
        LogV3.info("Test 11a passed: Preflight request with forbidden origin correctly blocked");
    }

    /**
     * Test 11b: Preflight request with forbidden method should be blocked
     */
    private void testCorsPreflightRequestBlockedForbiddenMethod() throws Exception {
        LogV3.info("Test 11b: Preflight request with forbidden method should be blocked");
        // Create and configure CORS handler for this test - only allow GET and POST, not PUT
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";
        this.lastServerException = null;
        // Request PUT method which is not allowed
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "PUT").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Preflight request with forbidden method should return 403 Forbidden, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 11b passed: Preflight request with forbidden method correctly blocked");
    }

    /**
     * Test 11c: Preflight request with various allowed methods
     */
    private void testCorsPreflightRequestWithVariousMethods() throws Exception {
        LogV3.info("Test 11c: Preflight request with various allowed methods");
        // Create and configure CORS handler for this test - allow multiple methods
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST, RequestMethod.PUT, RequestMethod.DELETE, RequestMethod.PATCH));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";
        // Test GET method
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "GET").setUrl(url));
        int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request for GET should return 200/204, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed method GET", context);
        // Test POST method
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").setUrl(url));
        responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request for POST should return 200/204, was: " + responseCode, context);
        // Test PUT method
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "PUT").setUrl(url));
        responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request for PUT should return 200/204, was: " + responseCode, context);
        // Test DELETE method
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "DELETE").setUrl(url));
        responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request for DELETE should return 200/204, was: " + responseCode, context);
        // Verify Access-Control-Allow-Methods contains all methods
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        this.assertTrueWithContext(allowMethods != null, "Access-Control-Allow-Methods should be present", context);
        this.assertTrueWithContext(allowMethods.contains("GET") && allowMethods.contains("POST") && allowMethods.contains("PUT") && allowMethods.contains("DELETE"), "Access-Control-Allow-Methods should contain all allowed methods, was: " + allowMethods, context);
        LogV3.info("Test 11c passed: Preflight requests with various methods work correctly");
    }

    /**
     * Test 11d: Preflight request with various headers
     */
    private void testCorsPreflightRequestWithVariousHeaders() throws Exception {
        LogV3.info("Test 11d: Preflight request with various headers");
        // Create and configure CORS handler for this test
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";
        // Test with single header
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request with single header should return 200/204, was: " + responseCode, context);
        String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        this.assertTrueWithContext(allowHeaders != null && allowHeaders.contains("Content-Type"), "Access-Control-Allow-Headers should contain Content-Type, was: " + allowHeaders, context);
        // Test with multiple headers
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type, Authorization, X-Custom-Header").setUrl(url));
        responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request with multiple headers should return 200/204, was: " + responseCode, context);
        allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        this.assertTrueWithContext(allowHeaders != null, "Access-Control-Allow-Headers should be present", context);
        this.assertTrueWithContext(allowHeaders.contains("Content-Type") && allowHeaders.contains("Authorization") && allowHeaders.contains("X-Custom-Header"), "Access-Control-Allow-Headers should contain all requested headers, was: " + allowHeaders, context);
        LogV3.info("Test 11d passed: Preflight requests with various headers work correctly");
    }

    /**
     * Test 11e: Preflight request without Origin header should be handled normally
     */
    private void testCorsPreflightRequestWithoutOrigin() throws Exception {
        LogV3.info("Test 11e: Preflight request without Origin header should be handled normally");
        // Create and configure CORS handler for this test
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // OPTIONS request without Origin header
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Access-Control-Request-Method", "POST").setUrl(url));
        final int responseCode = context.getCode();
        // Without Origin, it's not a CORS request, so it should be handled normally (may return 200, 204, or 501 depending on handler)
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204 || responseCode == 501, "OPTIONS request without Origin should return 200/204/501, was: " + responseCode, context);
        // CORS headers should not be present (no Origin = no CORS)
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin == null, "Access-Control-Allow-Origin should not be present for request without Origin", context);
        LogV3.info("Test 11e passed: Preflight request without Origin handled correctly");
    }

    /**
     * Test 11f: Preflight request with wildcard origin
     */
    private void testCorsPreflightRequestWithWildcardOrigin() throws Exception {
        LogV3.info("Test 11f: Preflight request with wildcard origin");
        // Create and configure CORS handler for this test - allow all origins
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("*")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://any-origin.com";
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request with wildcard origin should return 200/204, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for wildcard origin", context);
        // Access-Control-Allow-Origin should echo the requesting origin (not "*")
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null && allowOrigin.equals(origin), "Access-Control-Allow-Origin should echo requesting origin, was: " + allowOrigin, context);
        LogV3.info("Test 11f passed: Preflight request with wildcard origin works correctly");
    }

    /**
     * Test 11g: Preflight request with path-aware CORS (allowed)
     */
    private void testCorsPreflightRequestPathAware() throws Exception {
        LogV3.info("Test 11g: Preflight request with path-aware CORS (allowed)");
        // Create and configure CORS handler for this test - path-aware origin
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com", "/connect/probe")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/connect/probe";
        final String origin = "https://example.com";
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "Preflight request with matching path should return 200/204, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for matching path", context);
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null && allowOrigin.equals(origin), "Access-Control-Allow-Origin should be present and match origin, was: " + allowOrigin, context);
        LogV3.info("Test 11g passed: Preflight request with path-aware CORS (allowed) works correctly");
    }

    /**
     * Test 11h: Preflight request with path-aware CORS (blocked)
     */
    private void testCorsPreflightRequestPathAwareBlocked() throws Exception {
        LogV3.info("Test 11h: Preflight request with path-aware CORS (blocked)");
        // Create and configure CORS handler for this test - path-aware origin
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(Arrays.asList(new OriginRule("https://example.com", "/connect/probe")));
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.SECONDS.toMillis(30));
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        final String url = "http://localhost:" + this.serverPort + "/connect/other";
        final String origin = "https://example.com";
        this.lastServerException = null;
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Preflight request with non-matching path should return 403 Forbidden, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // CORS headers should not be present
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin == null, "Access-Control-Allow-Origin should not be present for blocked path", context);
        LogV3.info("Test 11h passed: Preflight request with path-aware CORS (blocked) correctly blocked");
    }
}
