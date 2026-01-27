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

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.CorsHandler;

/**
 * Tests for HTTP server CORS (Cross-Origin Resource Sharing) functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Cross-origin requests are blocked by default (when CORS is disabled)</li>
 * <li>Direct browser navigation (no Origin) is allowed</li>
 * <li>Origin requests are blocked by default (even localhost) when CORS is disabled</li>
 * <li>Empty or invalid Origin headers are rejected when CORS is disabled</li>
 * <li>No CORS headers are present when requests are blocked</li>
 * <li>CORS headers are correctly set when CORS is enabled</li>
 * <li>Access-Control-Allow-Origin echoes the Origin value from request</li>
 * <li>Access-Control-Allow-Headers is dynamically taken from request</li>
 * <li>All CORS headers (Allow-Origin, Allow-Methods, Allow-Headers, Max-Age, Expose-Headers) work correctly</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerCorsTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        java.util.Set<RequestMethod> previousMethods = null;
        try {
            this.setupServer();
            // Allow OPTIONS for CORS preflight tests (and keep GET/POST for existing tests)
            previousMethods = this.allowHttpMethods(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST);
            this.testCrossOriginRequestFromForeignDomainBlocked();
            this.testDirectBrowserNavigationAllowed();
            this.testLocalhostOriginBlocked();
            this.testEmptyOriginHeaderBlocked();
            this.testWhitespaceOriginHeaderBlocked();
            this.testNullStringOriginBlocked();

            // Enable CORS and test CORS functionality
            this.enableCors();
            this.testCorsRejectsNotAllowedOrigin();
            this.testCorsHeadersPresentForCrossOriginRequest();
            this.testCorsAllowOriginEchoesRequestOrigin();
            this.testCorsAllowHeadersFromRequest();
            this.testCorsAllowMethods();
            this.testCorsMaxAge();
            this.testCorsExposeHeaders();
            this.testCorsPreflightRequest();
            this.testCorsNoOriginNoCorsHeaders();

            // CORS wildcard: allow any origin, send "*"
            this.enableCorsAllowAll();
            this.testCorsAllowAllOrigins();

            // CORS with allowedOrigins=null: reject all Origin requests (even localhost)
            this.enableCorsForbidAllOrigins();
            this.testCorsRejectsAllOrigins();
            this.testCorsForbidAllAllowsNoOrigin();

            // Pattern-based rules tests
            this.testCredentialsPatternRules();
            this.testCredentialsPatternRulesPriority();
            this.testCredentialsPatternRulesDefaultDeny();
            this.testPrivateNetworkPatternRules();
            this.testPrivateNetworkPatternRulesPriority();
            this.testPrivateNetworkPatternRulesDefaultDeny();
            this.testCredentialsPatternRulesInHttpResponse();
            this.testPrivateNetworkPatternRulesInHttpResponse();
            this.testCorsHandlerValidation();
            this.testCorsHandlerValidationDisabled();
        } finally {
            if (previousMethods != null) {
                this.restoreHttpMethods(previousMethods);
            }
            this.teardownServer();
        }
    }

    /**
     * Enable CORS configuration on the server (like RemoteAPI.java behavior)
     *
     * <p>
     * This method configures CORS with explicit allowed origins.
     * </p>
     *
     * <p>
     * <b>Note:</b> With allowedOrigins=null, all Origin requests are forbidden. For production use with specific allowed origins, use:
     * </p>
     * <ul>
     * <li>corsHandler.setAllowedOrigins(java.util.Collections.singleton("https://example.com"))</li>
     * </ul>
     */
    private void enableCors() {
        // Explicitly allow example.com (and an additional origin to cover multiple origins)
        final Set<String> allowedOrigins = new HashSet<String>();
        allowedOrigins.add("https://example.com");
        allowedOrigins.add("https://another.example");
        this.configureCors(allowedOrigins);
        LogV3.info("CORS enabled on server with explicit allowed origins");
    }

    /**
     * Enable CORS for all origins using wildcard "*"
     */
    private void enableCorsAllowAll() {
        final Set<String> allowedOrigins = new HashSet<String>();
        allowedOrigins.add("*");
        this.configureCors(allowedOrigins);
        LogV3.info("CORS enabled on server for all origins (*)");
    }

    /**
     * Enable CORS with allowedOrigins=null (reject all Origin requests)
     */
    private void enableCorsForbidAllOrigins() {
        this.configureCors(null);
        LogV3.info("CORS enabled on server with allowedOrigins=null (reject all Origin requests)");
    }

    /**
     * Common CORS configuration shared by the tests.
     */
    private void configureCors(final Set<String> allowedOrigins) {
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setExposeHeaders("X-Custom-Header, X-Another-Header");
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(30000L); // 30 seconds in milliseconds, like RemoteAPI.java
        corsHandler.setAllowHeadersFromRequest(true); // Dynamic allowHeaders from request (State-of-the-Art)
        httpServer.setCorsHandler(corsHandler);
    }

    private void assertNoCorsHeaders(final RequestContext context, final String messagePrefix) throws Exception {
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);

        this.assertTrueWithContext(allowOrigin == null, messagePrefix + ": Access-Control-Allow-Origin header should not be present, but was: " + allowOrigin, context);
        this.assertTrueWithContext(allowMethods == null, messagePrefix + ": Access-Control-Allow-Methods header should not be present, but was: " + allowMethods, context);
        this.assertTrueWithContext(allowHeaders == null, messagePrefix + ": Access-Control-Allow-Headers header should not be present, but was: " + allowHeaders, context);
        this.assertTrueWithContext(maxAge == null, messagePrefix + ": Access-Control-Max-Age header should not be present, but was: " + maxAge, context);
    }

    /**
     * Test 1: Cross-origin request from foreign domain should be blocked
     */
    private void testCrossOriginRequestFromForeignDomainBlocked() throws Exception {
        LogV3.info("Test 1: Cross-origin request from foreign domain blocked");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://boeseseite.com").setUrl(url));
        final int responseCode = context.getCode();

        // Cross-origin requests should be rejected with 403 Forbidden
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Cross-Origin request should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode, context);

        // Check that no CORS headers are present in the response (request was blocked)
        this.assertNoCorsHeaders(context, "Default CORS blocked request");

        LogV3.info("Test 1 passed: Cross-origin request blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 2: Direct browser navigation (no Origin) should be allowed
     */
    private void testDirectBrowserNavigationAllowed() throws Exception {
        LogV3.info("Test 2: Direct browser navigation (no Origin) allowed");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Don't set Origin header - should be allowed
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Direct browser navigation (no Origin) should return 200, was: " + responseCode, context);
        this.assertNoCorsHeaders(context, "Default CORS no Origin");
        LogV3.info("Test 2 passed: Direct browser navigation successful: " + responseCode);
    }

    /**
     * Test 3: Localhost Origin should be blocked when CORS is disabled
     */
    private void testLocalhostOriginBlocked() throws Exception {
        LogV3.info("Test 3: Localhost Origin blocked (CORS disabled)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://localhost:" + this.serverPort).setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Localhost Origin should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode, context);
        this.assertNoCorsHeaders(context, "Default CORS localhost Origin");
        LogV3.info("Test 3 passed: Localhost Origin blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 3a: Empty Origin header should be blocked when CORS is disabled
     */
    private void testEmptyOriginHeaderBlocked() throws Exception {
        this.assertOriginBlocked("", "Empty Origin header");
    }

    /**
     * Test 3b: Whitespace Origin header should be blocked when CORS is disabled
     */
    private void testWhitespaceOriginHeaderBlocked() throws Exception {
        this.assertOriginBlocked("   ", "Whitespace Origin header");
    }

    /**
     * Test 3c: Origin header "null" should be blocked when CORS is disabled
     */
    private void testNullStringOriginBlocked() throws Exception {
        this.assertOriginBlocked("null", "Origin header \"null\"");
    }

    private void assertOriginBlocked(final String originValue, final String label) throws Exception {
        LogV3.info("Test: " + label + " blocked (CORS disabled)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", originValue).setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), label + " should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode, context);
        this.assertNoCorsHeaders(context, "Default CORS " + label);
        LogV3.info("Test passed: " + label + " blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 4: Cross-origin request from non-allowed domain should be blocked even when CORS is enabled
     */
    private void testCorsRejectsNotAllowedOrigin() throws Exception {
        LogV3.info("Test 4: Cross-origin request from non-allowed domain blocked (CORS enabled)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://boeseseite.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));
        final int responseCode = context.getCode();

        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Non-allowed Origin should return " + ResponseCode.ERROR_FORBIDDEN.getCode() + " Forbidden, was: " + responseCode, context);

        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);

        this.assertTrueWithContext(allowOrigin == null, "Access-Control-Allow-Origin header should not be present for blocked Origin, but was: " + allowOrigin, context);
        this.assertTrueWithContext(allowMethods == null, "Access-Control-Allow-Methods header should not be present for blocked Origin, but was: " + allowMethods, context);
        this.assertTrueWithContext(allowHeaders == null, "Access-Control-Allow-Headers header should not be present for blocked Origin, but was: " + allowHeaders, context);
        this.assertTrueWithContext(maxAge == null, "Access-Control-Max-Age header should not be present for blocked Origin, but was: " + maxAge, context);

        LogV3.info("Test 4 passed: Non-allowed Origin blocked with " + responseCode + ", no CORS headers present");
    }

    /**
     * Test 5: CORS headers should be present for cross-origin requests when CORS is enabled
     *
     * <p>
     * This test verifies that when CORS is enabled with explicit allowed origins, the CORS headers are correctly set for allowed
     * cross-origin requests.
     * </p>
     *
     * <p>
     * Note: With allowedOrigins=null, all Origin requests are forbidden.
     * </p>
     */
    private void testCorsHeadersPresentForCrossOriginRequest() throws Exception {
        LogV3.info("Test 5: CORS headers present for cross-origin request (CORS enabled)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));
        final int responseCode = context.getCode();

        // Request should succeed (origin is explicitly allowed)
        this.assertTrueWithContext(responseCode == 200, "Cross-origin request with CORS enabled should return 200, was: " + responseCode, context);

        // Check that CORS headers are present
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null, "Access-Control-Allow-Origin header should be present when CORS is enabled", context);
        // Origin is explicitly allowed, so Access-Control-Allow-Origin should match it
        assertEquals(origin, allowOrigin, "Access-Control-Allow-Origin should match the allowed Origin from request");

        LogV3.info("Test 5 passed: CORS headers present for cross-origin request, Origin allowed: " + allowOrigin);
    }

    /**
     * Test 6: Access-Control-Allow-Origin should echo the Origin value from request
     */
    private void testCorsAllowOriginEchoesRequestOrigin() throws Exception {
        LogV3.info("Test 6: Access-Control-Allow-Origin echoes request Origin");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));
        final int responseCode = context.getCode();

        this.assertTrueWithContext(responseCode == 200, "Request should return 200, was: " + responseCode, context);

        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null, "Access-Control-Allow-Origin header should be present", context);
        assertEquals(origin, allowOrigin, "Access-Control-Allow-Origin should match the allowed Origin from request");

        LogV3.info("Test 6 passed: Access-Control-Allow-Origin matches request Origin: " + allowOrigin);
    }

    /**
     * Test 7: Access-Control-Allow-Headers should be dynamically taken from Access-Control-Request-Headers
     */
    private void testCorsAllowHeadersFromRequest() throws Exception {
        LogV3.info("Test 7: Access-Control-Allow-Headers from request");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";
        final String requestedHeaders = "X-Custom-Header, Authorization";

        // Make a preflight OPTIONS request with Access-Control-Request-Headers
        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", requestedHeaders).setUrl(url));

        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "OPTIONS preflight request should return 200/204, was: " + responseCode, context);

        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        this.assertTrueWithContext(allowHeaders != null, "Access-Control-Allow-Headers header should be present", context);
        assertEquals(requestedHeaders, allowHeaders, "Access-Control-Allow-Headers should echo Access-Control-Request-Headers from request");

        LogV3.info("Test 7 passed: Access-Control-Allow-Headers from request: " + allowHeaders);
    }

    /**
     * Test 8: Access-Control-Allow-Methods should be set correctly
     */
    private void testCorsAllowMethods() throws Exception {
        LogV3.info("Test 8: Access-Control-Allow-Methods");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").setUrl(url));

        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "OPTIONS request should return 200/204, was: " + responseCode, context);

        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        this.assertTrueWithContext(allowMethods != null, "Access-Control-Allow-Methods header should be present", context);
        this.assertTrueWithContext(allowMethods.contains("OPTIONS") && allowMethods.contains("GET") && allowMethods.contains("POST"), "Access-Control-Allow-Methods should contain OPTIONS, GET, POST, was: " + allowMethods, context);

        LogV3.info("Test 8 passed: Access-Control-Allow-Methods: " + allowMethods);
    }

    /**
     * Test 9: Access-Control-Max-Age should be set correctly
     */
    private void testCorsMaxAge() throws Exception {
        LogV3.info("Test 9: Access-Control-Max-Age");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").setUrl(url));

        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "OPTIONS request should return 200/204, was: " + responseCode, context);

        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);
        this.assertTrueWithContext(maxAge != null, "Access-Control-Max-Age header should be present", context);
        assertEquals("30", maxAge, "Access-Control-Max-Age should be 30 seconds (converted from 30000 ms)");

        LogV3.info("Test 9 passed: Access-Control-Max-Age: " + maxAge);
    }

    /**
     * Test 10: Access-Control-Expose-Headers should be set correctly
     */
    private void testCorsExposeHeaders() throws Exception {
        LogV3.info("Test 10: Access-Control-Expose-Headers");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));

        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "GET request should return 200, was: " + responseCode, context);

        final String exposeHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS);
        this.assertTrueWithContext(exposeHeaders != null, "Access-Control-Expose-Headers header should be present", context);
        this.assertTrueWithContext(exposeHeaders.contains("X-Custom-Header") && exposeHeaders.contains("X-Another-Header"), "Access-Control-Expose-Headers should contain configured headers, was: " + exposeHeaders, context);

        LogV3.info("Test 10 passed: Access-Control-Expose-Headers: " + exposeHeaders);
    }

    /**
     * Test 11: CORS preflight request (OPTIONS) should work correctly
     */
    private void testCorsPreflightRequest() throws Exception {
        LogV3.info("Test 11: CORS preflight request (OPTIONS)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://example.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", origin).addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Headers", "Content-Type, Authorization").setUrl(url));

        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200 || responseCode == 204, "OPTIONS preflight request should return 200/204, was: " + responseCode, context);

        // Check all CORS headers are present
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        final String allowMethods = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        final String allowHeaders = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        final String maxAge = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);

        this.assertTrueWithContext(allowOrigin != null, "Access-Control-Allow-Origin should be present in preflight response", context);
        this.assertTrueWithContext(allowMethods != null, "Access-Control-Allow-Methods should be present in preflight response", context);
        this.assertTrueWithContext(allowHeaders != null, "Access-Control-Allow-Headers should be present in preflight response", context);
        this.assertTrueWithContext(maxAge != null, "Access-Control-Max-Age should be present in preflight response", context);

        assertEquals(origin, allowOrigin, "Access-Control-Allow-Origin should match Origin");
        assertEquals("Content-Type, Authorization", allowHeaders, "Access-Control-Allow-Headers should echo request headers");

        LogV3.info("Test 11 passed: CORS preflight request successful");
    }

    /**
     * Test 12: No CORS headers should be set when there's no Origin header
     */
    private void testCorsNoOriginNoCorsHeaders() throws Exception {
        LogV3.info("Test 12: No CORS headers when no Origin header");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Request without Origin header
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Request without Origin should return 200, was: " + responseCode, context);

        // CORS headers should not be present (no Origin = no CORS)
        this.assertNoCorsHeaders(context, "CORS enabled no Origin");

        LogV3.info("Test 12 passed: No CORS headers when no Origin header");
    }

    /**
     * Test 13: Wildcard CORS should allow any Origin and return "*"
     */
    private void testCorsAllowAllOrigins() throws Exception {
        LogV3.info("Test 13: Wildcard CORS allows any Origin");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String origin = "https://boeseseite.com";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", origin).setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Wildcard CORS should allow any Origin, was: " + responseCode, context);

        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        assertEquals("*", allowOrigin, "Wildcard CORS should return Access-Control-Allow-Origin: *");
        this.assertTrueWithContext(!origin.equals(allowOrigin), "Wildcard CORS should not echo the Origin when using '*'", context);

        LogV3.info("Test 13 passed: Wildcard CORS allowed Origin with Access-Control-Allow-Origin=*");
    }

    /**
     * Test 14: allowedOrigins=null should reject all Origin requests (including localhost)
     */
    private void testCorsRejectsAllOrigins() throws Exception {
        LogV3.info("Test 14: allowedOrigins=null rejects all Origin requests");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://localhost:" + this.serverPort).setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "allowedOrigins=null should reject Origin requests, was: " + responseCode, context);
        this.assertNoCorsHeaders(context, "allowedOrigins=null blocked request");

        LogV3.info("Test 14 passed: Origin request rejected with allowedOrigins=null");
    }

    /**
     * Test 15: allowedOrigins=null should still allow requests without Origin
     */
    private void testCorsForbidAllAllowsNoOrigin() throws Exception {
        LogV3.info("Test 15: allowedOrigins=null allows no-Origin requests");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "No-Origin request should be allowed with allowedOrigins=null, was: " + responseCode, context);
        this.assertNoCorsHeaders(context, "allowedOrigins=null no Origin");

        LogV3.info("Test 15 passed: No-Origin request allowed with allowedOrigins=null");
    }

    /**
     * Test 16: Pattern-based credentials rules - basic functionality
     */
    private void testCredentialsPatternRules() throws Exception {
        LogV3.info("Test 16: Pattern-based credentials rules");
        final CorsHandler corsHandler = new CorsHandler();
        // Disable validation for this test - we're testing pattern rules functionality, not validation
        corsHandler.setEnableSecurityValidation(false);
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Test: No rules - should return false (default deny)
        assertFalse(corsHandler.isAllowCredentials("https://example.com"), "No rules should deny credentials");

        // Test: Add rule to allow specific origin
        corsHandler.addCredentialsRule(Pattern.compile("https://example\\.com"), true);
        assertTrue(corsHandler.isAllowCredentials("https://example.com"), "Rule should allow credentials for matching origin");
        assertFalse(corsHandler.isAllowCredentials("https://other.com"), "Rule should deny credentials for non-matching origin");

        // Test: Add rule to deny all
        corsHandler.addCredentialsRule(Pattern.compile(".*"), false);
        assertFalse(corsHandler.isAllowCredentials("https://other.com"), "Deny-all rule should deny credentials");

        LogV3.info("Test 16 passed: Pattern-based credentials rules work correctly");
    }

    /**
     * Test 17: Pattern-based credentials rules - priority (last rule has highest priority)
     */
    private void testCredentialsPatternRulesPriority() throws Exception {
        LogV3.info("Test 17: Pattern-based credentials rules priority");
        final CorsHandler corsHandler = new CorsHandler();
        // Disable validation for this test - we're testing pattern rules functionality, not validation
        corsHandler.setEnableSecurityValidation(false);
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Add deny-all rule first
        corsHandler.addCredentialsRule(Pattern.compile(".*"), false);
        // Add allow rule for specific origin (higher priority - checked last)
        corsHandler.addCredentialsRule(Pattern.compile("https://example\\.com"), true);

        // Test: Specific origin should be allowed (last rule wins)
        assertTrue(corsHandler.isAllowCredentials("https://example.com"), "Last rule should have highest priority - example.com should be allowed");

        // Test: Other origins should be denied (first rule applies)
        assertFalse(corsHandler.isAllowCredentials("https://other.com"), "Other origins should be denied");

        // Test: Multiple specific rules - last one wins
        corsHandler.addCredentialsRule(Pattern.compile("https://trusted\\.com"), true);
        corsHandler.addCredentialsRule(Pattern.compile("https://trusted\\.com"), false); // Override
        assertFalse(corsHandler.isAllowCredentials("https://trusted.com"), "Last rule for trusted.com should deny");

        LogV3.info("Test 17 passed: Pattern-based credentials rules priority works correctly");
    }

    /**
     * Test 18: Pattern-based credentials rules - default deny when no rule matches
     */
    private void testCredentialsPatternRulesDefaultDeny() throws Exception {
        LogV3.info("Test 18: Pattern-based credentials rules default deny");
        final CorsHandler corsHandler = new CorsHandler();
        // Disable validation for this test - we're testing pattern rules functionality, not validation
        corsHandler.setEnableSecurityValidation(false);
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Add rule for specific pattern
        corsHandler.addCredentialsRule(Pattern.compile("https://example\\.com"), true);

        // Test: Matching origin should be allowed
        assertTrue(corsHandler.isAllowCredentials("https://example.com"), "Matching origin should be allowed");

        // Test: Non-matching origin should be denied (default)
        assertFalse(corsHandler.isAllowCredentials("https://other.com"), "Non-matching origin should be denied by default");

        // Test: Clear rules - should default to deny (false)
        corsHandler.clearCredentialsRules();
        assertFalse(corsHandler.isAllowCredentials("https://example.com"), "After clearing rules, should default to deny (false)");

        // Test: Add Pattern.compile(".*"):true rule to allow credentials for all origins
        corsHandler.addCredentialsRule(Pattern.compile(".*"), true);
        // This should now allow all origins
        assertTrue(corsHandler.isAllowCredentials("https://example.com"), "After adding \".*\":true rule, should allow all origins");
        assertTrue(corsHandler.isAllowCredentials("https://other.com"), "After adding \".*\":true rule, should allow all origins");

        LogV3.info("Test 18 passed: Pattern-based credentials rules default deny works correctly");
    }

    /**
     * Test 19: Pattern-based private network access rules - basic functionality
     */
    private void testPrivateNetworkPatternRules() throws Exception {
        LogV3.info("Test 19: Pattern-based private network access rules");
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Test: No rules - should return false (default deny)
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "No rules should deny private network access");

        // Test: Add rule to allow specific origin
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile("https://example\\.com"), true);
        assertTrue(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "Rule should allow private network access for matching origin");
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://other.com"), "Rule should deny private network access for non-matching origin");

        // Test: Add rule to deny all
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile(".*"), false);
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://other.com"), "Deny-all rule should deny private network access");

        LogV3.info("Test 19 passed: Pattern-based private network access rules work correctly");
    }

    /**
     * Test 20: Pattern-based private network access rules - priority (last rule has highest priority)
     */
    private void testPrivateNetworkPatternRulesPriority() throws Exception {
        LogV3.info("Test 20: Pattern-based private network access rules priority");
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Add deny-all rule first
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile(".*"), false);
        // Add allow rule for specific origin (higher priority - checked last)
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile("https://example\\.com"), true);

        // Test: Specific origin should be allowed (last rule wins)
        assertTrue(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "Last rule should have highest priority - example.com should be allowed");

        // Test: Other origins should be denied (first rule applies)
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://other.com"), "Other origins should be denied");

        LogV3.info("Test 20 passed: Pattern-based private network access rules priority works correctly");
    }

    /**
     * Test 21: Pattern-based private network access rules - default deny when no rule matches
     */
    private void testPrivateNetworkPatternRulesDefaultDeny() throws Exception {
        LogV3.info("Test 21: Pattern-based private network access rules default deny");
        final CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        // Add rule for specific pattern
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile("https://example\\.com"), true);

        // Test: Matching origin should be allowed
        assertTrue(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "Matching origin should be allowed");

        // Test: Non-matching origin should be denied (default)
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://other.com"), "Non-matching origin should be denied by default");

        // Test: Clear rules - should default to deny (false)
        corsHandler.clearPrivateNetworkRequestRules();
        assertFalse(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "After clearing rules, should default to deny (false)");

        // Test: Add Pattern.compile(".*"):true rule to allow private network access for all origins
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile(".*"), true);
        // This should now allow all origins
        assertTrue(corsHandler.isAllowPrivateNetworkFromRequest("https://example.com"), "After adding \".*\":true rule, should allow all origins");
        assertTrue(corsHandler.isAllowPrivateNetworkFromRequest("https://other.com"), "After adding \".*\":true rule, should allow all origins");

        LogV3.info("Test 21 passed: Pattern-based private network access rules default deny works correctly");
    }

    /**
     * Test 22: Pattern-based credentials rules in HTTP response
     */
    private void testCredentialsPatternRulesInHttpResponse() throws Exception {
        LogV3.info("Test 22: Pattern-based credentials rules in HTTP response");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Configure CORS with pattern-based credentials rules
        final CorsHandler corsHandler = new CorsHandler();
        final Set<String> allowedOrigins = new HashSet<String>();
        allowedOrigins.add("https://example.com");
        allowedOrigins.add("https://trusted.com");
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);

        // Deny credentials for all by default
        corsHandler.addCredentialsRule(Pattern.compile(".*"), false);
        // Allow credentials for trusted.com only
        corsHandler.addCredentialsRule(Pattern.compile("https://trusted\\.com"), true);

        this.httpServer.setCorsHandler(corsHandler);

        // Test: Request from example.com - credentials should NOT be allowed
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(url));
        final String allowCredentials1 = context.getConnection().getHeaderField(HTTPConstants.ACCESS_CONTROL_ALLOW_CREDENTIALS);
        this.assertTrueWithContext(allowCredentials1 == null, "Credentials header should not be present for example.com, but was: " + allowCredentials1, context);

        // Test: Request from trusted.com - credentials SHOULD be allowed
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://trusted.com").setUrl(url));
        final String allowCredentials2 = context.getConnection().getHeaderField(HTTPConstants.ACCESS_CONTROL_ALLOW_CREDENTIALS);
        this.assertTrueWithContext("true".equals(allowCredentials2), "Credentials header should be 'true' for trusted.com, but was: " + allowCredentials2, context);

        LogV3.info("Test 22 passed: Pattern-based credentials rules work in HTTP response");
    }

    /**
     * Test 23: Pattern-based private network access rules in HTTP response
     */
    private void testPrivateNetworkPatternRulesInHttpResponse() throws Exception {
        LogV3.info("Test 23: Pattern-based private network access rules in HTTP response");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        // Configure CORS with pattern-based private network access rules
        final CorsHandler corsHandler = new CorsHandler();
        final Set<String> allowedOrigins = new HashSet<String>();
        allowedOrigins.add("https://example.com");
        allowedOrigins.add("https://trusted.com");
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);

        // Deny private network access for all by default
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile(".*"), false);
        // Allow private network access for trusted.com only
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile("https://trusted\\.com"), true);

        this.httpServer.setCorsHandler(corsHandler);

        // Test: Preflight request from example.com with Access-Control-Request-Private-Network - should NOT be allowed
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", "https://example.com").addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Private-Network", "true").setUrl(url));
        final String allowPrivateNetwork1 = context.getConnection().getHeaderField(HTTPConstants.ACCESS_CONTROL_ALLOW_PRIVATE_NETWORK);
        this.assertTrueWithContext(allowPrivateNetwork1 == null, "Private Network Access header should not be present for example.com, but was: " + allowPrivateNetwork1, context);

        // Test: Preflight request from trusted.com with Access-Control-Request-Private-Network - SHOULD be allowed
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).addHeader("Origin", "https://trusted.com").addHeader("Access-Control-Request-Method", "POST").addHeader("Access-Control-Request-Private-Network", "true").setUrl(url));
        final String allowPrivateNetwork2 = context.getConnection().getHeaderField(HTTPConstants.ACCESS_CONTROL_ALLOW_PRIVATE_NETWORK);
        this.assertTrueWithContext("true".equals(allowPrivateNetwork2), "Private Network Access header should be 'true' for trusted.com, but was: " + allowPrivateNetwork2, context);

        LogV3.info("Test 23 passed: Pattern-based private network access rules work in HTTP response");
    }

    /**
     * Test 24: CORS handler validation - dangerous configurations
     */
    private void testCorsHandlerValidation() throws Exception {
        LogV3.info("Test 24: CORS handler validation");
        final CorsHandler corsHandler = new CorsHandler();

        // Test: "*" origin with credentials rule that allows credentials should throw exception
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));

        try {
            corsHandler.addCredentialsRule((Pattern.compile(".*")), true);

            AWTest.assertFalse(true, "Should throw IllegalStateException when \"*\" origin is combined with credentials rule that allows credentials");
        } catch (IllegalStateException e) {
            // Expected
            assertTrue(e.getMessage().contains("*") && e.getMessage().contains("credentials"), "Exception message should mention \"*\" and credentials");
        }

        // Test: "*" origin with credentials rule that denies credentials should be OK
        corsHandler.clearCredentialsRules();
        corsHandler.addCredentialsRule((Pattern.compile(".*")), false);
        corsHandler.validate(); // Should not throw

        // Test: Specific origin with credentials rule that allows credentials should be OK
        final Set<String> specificOrigins = new HashSet<String>();
        specificOrigins.add("https://example.com");
        corsHandler.setAllowedOrigins(specificOrigins);
        corsHandler.clearCredentialsRules();
        corsHandler.addCredentialsRule(Pattern.compile("https://example\\.com"), true);
        corsHandler.validate(); // Should not throw

        LogV3.info("Test 24 passed: CORS handler validation works correctly");
    }

    /**
     * Test 25: CORS handler validation disabled
     */
    private void testCorsHandlerValidationDisabled() throws Exception {
        LogV3.info("Test 25: CORS handler validation disabled");
        final CorsHandler corsHandler = new CorsHandler();

        // Disable security validation
        corsHandler.setEnableSecurityValidation(false);

        // Test: "*" origin with credentials rule that allows credentials should NOT throw when validation is disabled
        corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));
        corsHandler.addCredentialsRule(Pattern.compile(".*"), true);
        corsHandler.validate(); // Should not throw when validation is disabled

        // Re-enable validation
        corsHandler.setEnableSecurityValidation(true);
        try {
            corsHandler.validate();
            AWTest.assertFalse(true, "Should throw IllegalStateException when validation is re-enabled");
        } catch (IllegalStateException e) {
            // Expected
        }

        LogV3.info("Test 25 passed: CORS handler validation can be disabled");
    }
}
