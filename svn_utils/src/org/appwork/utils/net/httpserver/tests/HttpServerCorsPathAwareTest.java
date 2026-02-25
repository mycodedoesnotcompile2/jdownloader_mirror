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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.regex.Pattern;

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
 * Path-aware CORS tests for HTTP server.
 *
 * <p>
 * This test class covers all aspects of path-aware CORS functionality, including:
 * </p>
 * <ul>
 * <li>Origin with path restrictions</li>
 * <li>Wildcard path patterns</li>
 * <li>Path matching (exact and regex)</li>
 * <li>Trailing slash behavior</li>
 * <li>Path prefix restrictions</li>
 * </ul>
 *
 * <p>
 * The path {@code /connect/probe} is used in tests as a typical path-aware CORS example. In the Connect API,
 * the probe endpoint also requires valid authentication like all other API endpoints; this test only verifies
 * CORS path-aware behavior, not authentication.
 * </p>
 *
 * @author AppWork
 */
public class HttpServerCorsPathAwareTest extends HttpServerTestBase {
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
            // Path-aware CORS tests
            this.testPathAwareCorsOriginWithPath();
            this.testPathAwareCorsWildcardPath();
            this.testPathAwareCorsPathMatching();
            this.testPathAwareCorsPathRegex();
            this.testPathAwareCorsPathRestrictionBlocksWrongPath();
            this.testPathAwareCorsNoPathHeaderFallsBackToOriginOnly();
            this.testPathAwareCorsWildcardPathWithSpecificOrigin();
            this.testPathAwareCorsOriginWithTrailingSlashOnlyAllowsRoot();
            this.testPathAwareCorsOriginWithoutTrailingSlashAllowsAllPaths();
            this.testPathAwareCorsPathRestrictionWithWildcardOnlyAllowsPrefixPaths();
        } finally {
            if (previousMethods != null) {
                this.restoreHttpMethods(previousMethods);
            }
            this.teardownServer();
        }
    }

    /**
     * Test 26: Path-aware CORS - Origin with path restriction allows only matching path.
     * In the Connect API, /connect/probe also requires valid authentication; this test only checks CORS path rules.
     */
    private void testPathAwareCorsOriginWithPath() throws Exception {
        LogV3.info("Test 26: Path-aware CORS - Origin with path restriction");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with path-aware origins
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("https://example.com", "/connect/probe"));
        allowedOrigins.add(new OriginRule("https://example.com", "/api/v1"));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request to allowed path "/connect/probe" should succeed
        final String urlWithPath = "http://localhost:" + this.serverPort + "/connect/probe";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Request to allowed path should return 200, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin1 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin1 != null, "Access-Control-Allow-Origin should be present for allowed path", context);
        assertEquals("https://example.com", allowOrigin1, "Access-Control-Allow-Origin should match origin");
        // Test: Request to different allowed path "/api/v1" should succeed
        final String urlWithPath2 = "http://localhost:" + this.serverPort + "/api/v1";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath2));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Request to different allowed path should return 200, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin2 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin2 != null, "Access-Control-Allow-Origin should be present for allowed path", context);
        // Negative tests: Similar paths should be blocked
        // Test: "/connect/probeX" (extra character) should be blocked
        final String urlWrongPath1 = "http://localhost:" + this.serverPort + "/connect/probeX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath1));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        final String allowOrigin3 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin3 == null, "Access-Control-Allow-Origin should not be present for blocked path", context);
        // Test: "/connect/probe/" (trailing slash) should be blocked
        final String urlWrongPath2 = "http://localhost:" + this.serverPort + "/connect/probe/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath2));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with trailing slash should be blocked, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/extra" (sub-path) should be blocked
        final String urlWrongPath3 = "http://localhost:" + this.serverPort + "/connect/probe/extra";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath3));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Sub-path should be blocked, was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect" (prefix only) should be blocked
        final String urlWrongPath4 = "http://localhost:" + this.serverPort + "/connect";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath4));
        final int responseCode6 = context.getCode();
        this.assertTrueWithContext(responseCode6 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Prefix-only path should be blocked, was: " + responseCode6, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v1X" (extra character) should be blocked
        final String urlWrongPath5 = "http://localhost:" + this.serverPort + "/api/v1X";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath5));
        final int responseCode7 = context.getCode();
        this.assertTrueWithContext(responseCode7 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode7, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v10" (different version) should be blocked
        final String urlWrongPath6 = "http://localhost:" + this.serverPort + "/api/v10";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath6));
        final int responseCode8 = context.getCode();
        this.assertTrueWithContext(responseCode8 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Different version should be blocked, was: " + responseCode8, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 26 passed: Path-aware CORS with origin + path restrictions (including negative tests)");
    }

    /**
     * Test 27: Path-aware CORS - Wildcard path pattern allows any origin for matching path
     */
    private void testPathAwareCorsWildcardPath() throws Exception {
        LogV3.info("Test 27: Path-aware CORS - Wildcard path pattern");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with wildcard path pattern
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule(Pattern.compile(".*", Pattern.CASE_INSENSITIVE), (Pattern.compile("/connect/probe", Pattern.CASE_INSENSITIVE))));
        allowedOrigins.add(new OriginRule(Pattern.compile(".*", Pattern.CASE_INSENSITIVE), (Pattern.compile("/api/public", Pattern.CASE_INSENSITIVE))));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request from any origin to "/connect/probe" should succeed
        final String urlWithPath = "http://localhost:" + this.serverPort + "/connect/probe";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWithPath));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Wildcard path should allow any origin, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin1 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin1 != null, "Access-Control-Allow-Origin should be present for wildcard path", context);
        assertEquals("https://any-origin.com", allowOrigin1, "Wildcard path should return Access-Control-Allow-Origin with the requesting origin");
        // Test: Request from different origin to same path should also succeed
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://another-origin.com").setUrl(urlWithPath));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Wildcard path should allow any origin, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin2 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        assertEquals("https://another-origin.com", allowOrigin2, "Wildcard path should return Access-Control-Allow-Origin with the requesting origin");
        // Negative tests: Similar paths should be blocked
        // Test: "/connect/probeX" (extra character) should be blocked
        final String urlWrongPath1 = "http://localhost:" + this.serverPort + "/connect/probeX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath1));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/" (trailing slash) should be blocked
        final String urlWrongPath2 = "http://localhost:" + this.serverPort + "/connect/probe/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath2));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with trailing slash should be blocked, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/extra" (sub-path) should be blocked
        final String urlWrongPath3 = "http://localhost:" + this.serverPort + "/connect/probe/extra";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath3));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Sub-path should be blocked, was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect" (prefix only) should be blocked
        final String urlWrongPath4 = "http://localhost:" + this.serverPort + "/connect";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath4));
        final int responseCode6 = context.getCode();
        this.assertTrueWithContext(responseCode6 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Prefix-only path should be blocked, was: " + responseCode6, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/publicX" (extra character) should be blocked
        final String urlWrongPath5 = "http://localhost:" + this.serverPort + "/api/publicX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath5));
        final int responseCode7 = context.getCode();
        this.assertTrueWithContext(responseCode7 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode7, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 27 passed: Path-aware CORS with wildcard path patterns (including negative tests)");
    }

    /**
     * Test 28: Path-aware CORS - Path matching (exact match, case-insensitive)
     */
    private void testPathAwareCorsPathMatching() throws Exception {
        LogV3.info("Test 28: Path-aware CORS - Path matching");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with path-aware origin
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("https://example.com", "/Connect/Probe"));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Case-insensitive exact match should work
        final String urlWithPath = "http://localhost:" + this.serverPort + "/connect/probe";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Case-insensitive path match should succeed, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null, "Access-Control-Allow-Origin should be present for matching path", context);
        // Negative tests: Similar paths should be blocked
        // Test: "/connect/probeX" (extra character) should be blocked
        final String urlWrongPath1 = "http://localhost:" + this.serverPort + "/connect/probeX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath1));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/" (trailing slash) should be blocked
        final String urlWrongPath2 = "http://localhost:" + this.serverPort + "/connect/probe/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath2));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with trailing slash should be blocked, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/extra" (sub-path) should be blocked
        final String urlWrongPath3 = "http://localhost:" + this.serverPort + "/connect/probe/extra";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath3));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Sub-path should be blocked, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect" (prefix only) should be blocked
        final String urlWrongPath4 = "http://localhost:" + this.serverPort + "/connect";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath4));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Prefix-only path should be blocked, was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 28 passed: Path-aware CORS path matching (case-insensitive, including negative tests)");
    }

    /**
     * Test 29: Path-aware CORS - Path regex matching
     */
    private void testPathAwareCorsPathRegex() throws Exception {
        LogV3.info("Test 29: Path-aware CORS - Path regex matching");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with regex path pattern
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("https://example.com", Pattern.compile("/connect/.*")));
        allowedOrigins.add(new OriginRule(Pattern.compile(".*", Pattern.CASE_INSENSITIVE), (Pattern.compile("/api/v[0-9]+", Pattern.CASE_INSENSITIVE))));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Regex pattern "/connect/.*" should match "/connect/probe"
        final String urlWithPath1 = "http://localhost:" + this.serverPort + "/connect/probe";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath1));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Regex path pattern should match, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Regex pattern "/connect/.*" should match "/connect/api/v2"
        final String urlWithPath2 = "http://localhost:" + this.serverPort + "/connect/api/v2";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath2));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Regex path pattern should match sub-paths, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Wildcard regex pattern "* /api/v[0-9]+" (asterisk space slash) should match "/api/v1"
        final String urlWithPath3 = "http://localhost:" + this.serverPort + "/api/v1";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWithPath3));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == 200, "Wildcard regex path pattern should match, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Wildcard regex pattern "* /api/v[0-9]+" should match "/api/v99"
        final String urlWithPath4 = "http://localhost:" + this.serverPort + "/api/v99";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWithPath4));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == 200, "Wildcard regex path pattern should match version numbers, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Regex pattern "/connect/.*" should also match "/connect/" (.* matches zero or more characters)
        final String urlWithPath5 = "http://localhost:" + this.serverPort + "/connect/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath5));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == 200, "Regex path pattern /connect/.* should match /connect/ (.* matches zero characters), was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for /connect/, but got: " + this.lastServerException, context);
        // Negative tests for "/connect/.*" pattern: Paths that should NOT match
        // Test: "/connect" (without trailing slash) should be blocked
        final String urlWrongPath1 = "http://localhost:" + this.serverPort + "/connect";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath1));
        final int responseCodeWrong1 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong1 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path without trailing slash should be blocked for regex pattern /connect/.*, was: " + responseCodeWrong1, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connectX" (different prefix) should be blocked
        final String urlWrongPath3 = "http://localhost:" + this.serverPort + "/connectX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath3));
        final int responseCodeWrong3 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Different prefix should be blocked, was: " + responseCodeWrong3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Negative tests for "* /api/v[0-9]+" (asterisk space slash) pattern: Paths that should NOT match
        // Test: "/api/v" (without version number) should be blocked
        final String urlWrongPath4 = "http://localhost:" + this.serverPort + "/api/v";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath4));
        final int responseCodeWrong4 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path without version number should be blocked, was: " + responseCodeWrong4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/vX" (non-numeric version) should be blocked
        final String urlWrongPath5 = "http://localhost:" + this.serverPort + "/api/vX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath5));
        final int responseCodeWrong5 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with non-numeric version should be blocked, was: " + responseCodeWrong5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v10X" (version with extra character) should be blocked
        final String urlWrongPath6 = "http://localhost:" + this.serverPort + "/api/v10X";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath6));
        final int responseCodeWrong6 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong6 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with version and extra character should be blocked, was: " + responseCodeWrong6, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v1/" (trailing slash) should be blocked
        final String urlWrongPath7 = "http://localhost:" + this.serverPort + "/api/v1/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath7));
        final int responseCodeWrong7 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong7 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with trailing slash should be blocked, was: " + responseCodeWrong7, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v1/extra" (sub-path) should be blocked
        final String urlWrongPath8 = "http://localhost:" + this.serverPort + "/api/v1/extra";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWrongPath8));
        final int responseCodeWrong8 = context.getCode();
        this.assertTrueWithContext(responseCodeWrong8 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Sub-path should be blocked, was: " + responseCodeWrong8, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 29 passed: Path-aware CORS regex path matching (including negative tests)");
    }

    /**
     * Test 30: Path-aware CORS - Path restriction blocks wrong path
     */
    private void testPathAwareCorsPathRestrictionBlocksWrongPath() throws Exception {
        LogV3.info("Test 30: Path-aware CORS - Path restriction blocks wrong path");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with path-aware origin
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("https://example.com", "/connect/probe"));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request to wrong path should be blocked
        final String urlWithPath = "http://localhost:" + this.serverPort + "/connect/other";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == ResponseCode.ERROR_FORBIDDEN.getCode(), "Request to wrong path should return 403 Forbidden, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin == null, "Access-Control-Allow-Origin should not be present for blocked path, but was: " + allowOrigin, context);
        LogV3.info("Test 30 passed: Path-aware CORS blocks wrong path");
    }

    /**
     * Test 31: Path-aware CORS - No path header falls back to origin-only validation
     */
    private void testPathAwareCorsNoPathHeaderFallsBackToOriginOnly() throws Exception {
        LogV3.info("Test 31: Path-aware CORS - No path header falls back to origin-only validation");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with path-aware origin
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("https://example.com", "/connect/probe"));
        allowedOrigins.add(new OriginRule("https://example.com"));
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request without path restriction should fall back to origin-only check
        // Since "https://example.com" (without path) is in allowedOrigins, it should succeed
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(url));
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Request without path header should fall back to origin-only check, was: " + responseCode, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed origin, but got: " + this.lastServerException, context);
        final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin != null, "Access-Control-Allow-Origin should be present when origin matches", context);
        LogV3.info("Test 31 passed: Path-aware CORS falls back to origin-only validation when path header is missing");
    }

    /**
     * Test 32: Path-aware CORS - Wildcard path with specific origin (priority test)
     */
    private void testPathAwareCorsWildcardPathWithSpecificOrigin() throws Exception {
        LogV3.info("Test 32: Path-aware CORS - Wildcard path with specific origin");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with both wildcard path and specific origin+path
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule(Pattern.compile(".*", Pattern.CASE_INSENSITIVE), (Pattern.compile("/connect/probe", Pattern.CASE_INSENSITIVE)))); // Wildcard
                                                                                                                                                            // path
        allowedOrigins.add(new OriginRule("https://example.com", "/connect/probe")); // Specific origin+path
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Wildcard path should allow any origin
        final String urlWithPath = "http://localhost:" + this.serverPort + "/connect/probe";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://any-origin.com").setUrl(urlWithPath));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Wildcard path should allow any origin, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin1 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        assertEquals("https://any-origin.com", allowOrigin1, "Wildcard path should return Access-Control-Allow-Origin with the requesting origin");
        // Test: Specific origin+path should also work
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWithPath));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Specific origin+path should work, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        final String allowOrigin2 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        assertEquals("https://example.com", allowOrigin2, "Specific origin+path should return specific origin");
        // Negative tests: Similar paths should be blocked
        // Test: "/connect/probeX" (extra character) should be blocked
        final String urlWrongPath1 = "http://localhost:" + this.serverPort + "/connect/probeX";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath1));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with extra character should be blocked, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/" (trailing slash) should be blocked
        final String urlWrongPath2 = "http://localhost:" + this.serverPort + "/connect/probe/";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath2));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path with trailing slash should be blocked, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect/probe/extra" (sub-path) should be blocked
        final String urlWrongPath3 = "http://localhost:" + this.serverPort + "/connect/probe/extra";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath3));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Sub-path should be blocked, was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/connect" (prefix only) should be blocked
        final String urlWrongPath4 = "http://localhost:" + this.serverPort + "/connect";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "https://example.com").setUrl(urlWrongPath4));
        final int responseCode6 = context.getCode();
        this.assertTrueWithContext(responseCode6 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Prefix-only path should be blocked, was: " + responseCode6, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 32 passed: Path-aware CORS with wildcard path and specific origin (including negative tests)");
    }

    /**
     * Test 33: Path-aware CORS - Origin with trailing slash "/" should only allow root path
     */
    private void testPathAwareCorsOriginWithTrailingSlashOnlyAllowsRoot() throws Exception {
        LogV3.info("Test 33: Path-aware CORS - Origin with trailing slash only allows root path");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with origin that has trailing slash
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("http://example.com", "/")); // Trailing slash - should only allow root path
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request to root path "/" should succeed
        final String urlRoot = "http://localhost:" + this.serverPort + "/";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlRoot));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Request to root path should succeed with trailing slash origin, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed root path, but got: " + this.lastServerException, context);
        final String allowOrigin1 = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        this.assertTrueWithContext(allowOrigin1 != null, "Access-Control-Allow-Origin should be present for root path", context);
        assertEquals("http://example.com", allowOrigin1, "Access-Control-Allow-Origin should match origin");
        // Negative tests: Non-root paths should be blocked
        // Test: "/api" should be blocked
        final String urlApi = "http://localhost:" + this.serverPort + "/api";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api should be blocked for origin with trailing slash, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/test" should be blocked
        final String urlTest = "http://localhost:" + this.serverPort + "/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlTest));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /test should be blocked for origin with trailing slash, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api/v1" should be blocked
        final String urlApiV1 = "http://localhost:" + this.serverPort + "/api/v1";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApiV1));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api/v1 should be blocked for origin with trailing slash, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 33 passed: Origin with trailing slash only allows root path");
    }

    /**
     * Test 34: Path-aware CORS - Origin without trailing slash should allow all paths
     */
    private void testPathAwareCorsOriginWithoutTrailingSlashAllowsAllPaths() throws Exception {
        LogV3.info("Test 34: Path-aware CORS - Origin without trailing slash allows all paths");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with origin without trailing slash
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("http://example.com")); // No trailing slash - should allow all paths
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request to root path "/" should succeed
        final String urlRoot = "http://localhost:" + this.serverPort + "/";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlRoot));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Request to root path should succeed without trailing slash origin, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed root path, but got: " + this.lastServerException, context);
        // Test: Request to "/api" should succeed
        final String urlApi = "http://localhost:" + this.serverPort + "/api";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Request to /api should succeed without trailing slash origin, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Request to "/test" should succeed
        final String urlTest = "http://localhost:" + this.serverPort + "/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlTest));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == 200, "Request to /test should succeed without trailing slash origin, was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Request to "/api/v1" should succeed
        final String urlApiV1 = "http://localhost:" + this.serverPort + "/api/v1";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApiV1));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == 200, "Request to /api/v1 should succeed without trailing slash origin, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        LogV3.info("Test 34 passed: Origin without trailing slash allows all paths");
    }

    /**
     * Test 35: Path-aware CORS - Path restriction with wildcard should only allow paths under that prefix
     */
    private void testPathAwareCorsPathRestrictionWithWildcardOnlyAllowsPrefixPaths() throws Exception {
        LogV3.info("Test 35: Path-aware CORS - Path restriction with wildcard only allows prefix paths");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Configure CORS with path restriction using wildcard pattern
        final CorsHandler corsHandler = new CorsHandler();
        final List<OriginRule> allowedOrigins = new ArrayList<OriginRule>();
        allowedOrigins.add(new OriginRule("http://example.com", Pattern.compile("/api1/.*"))); // Regex pattern - should only allow paths
                                                                                               // under /api1/
        corsHandler.setAllowedOrigins(allowedOrigins);
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
        corsHandler.setMaxAge(30000L);
        corsHandler.setAllowHeadersFromRequest(true);
        this.httpServer.setCorsHandler(corsHandler);
        // Test: Request to "/api1/test" should succeed
        final String urlApi1Test = "http://localhost:" + this.serverPort + "/api1/test";
        this.lastServerException = null;
        RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi1Test));
        final int responseCode1 = context.getCode();
        this.assertTrueWithContext(responseCode1 == 200, "Request to /api1/test should succeed, was: " + responseCode1, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Test: Request to "/api1/v1/data" should succeed
        final String urlApi1V1 = "http://localhost:" + this.serverPort + "/api1/v1/data";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi1V1));
        final int responseCode2 = context.getCode();
        this.assertTrueWithContext(responseCode2 == 200, "Request to /api1/v1/data should succeed, was: " + responseCode2, context);
        this.assertTrueWithContext(this.lastServerException == null, "No server-side exception expected for allowed path, but got: " + this.lastServerException, context);
        // Negative tests: Paths outside /api1/ should be blocked
        // Test: "/api2/test" should be blocked
        final String urlApi2Test = "http://localhost:" + this.serverPort + "/api2/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi2Test));
        final int responseCode3 = context.getCode();
        this.assertTrueWithContext(responseCode3 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api2/test should be blocked (outside /api1/), was: " + responseCode3, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api1" (without trailing slash) should be blocked (pattern requires /api1/.*)
        final String urlApi1 = "http://localhost:" + this.serverPort + "/api1";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi1));
        final int responseCode4 = context.getCode();
        this.assertTrueWithContext(responseCode4 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api1 (without trailing content) should be blocked, was: " + responseCode4, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api10/test" should be blocked (different prefix)
        final String urlApi10Test = "http://localhost:" + this.serverPort + "/api10/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi10Test));
        final int responseCode5 = context.getCode();
        this.assertTrueWithContext(responseCode5 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api10/test should be blocked (different prefix), was: " + responseCode5, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/api1X/test" should be blocked (different prefix)
        final String urlApi1XTest = "http://localhost:" + this.serverPort + "/api1X/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlApi1XTest));
        final int responseCode6 = context.getCode();
        this.assertTrueWithContext(responseCode6 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /api1X/test should be blocked (different prefix), was: " + responseCode6, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        // Test: "/test" should be blocked (completely different path)
        final String urlTest = "http://localhost:" + this.serverPort + "/test";
        this.lastServerException = null;
        context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.GET).addHeader("Origin", "http://example.com").setUrl(urlTest));
        final int responseCode7 = context.getCode();
        this.assertTrueWithContext(responseCode7 == ResponseCode.ERROR_FORBIDDEN.getCode(), "Path /test should be blocked (outside /api1/), was: " + responseCode7, context);
        this.assertTrueWithContext(this.lastServerException instanceof ForbiddenOriginException, "Server-side exception should be ForbiddenOriginException, but was: " + this.lastServerException, context);
        LogV3.info("Test 35 passed: Path restriction with wildcard only allows prefix paths");
    }
}
