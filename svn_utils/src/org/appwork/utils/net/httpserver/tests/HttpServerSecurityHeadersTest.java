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

import java.net.URLEncoder;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl.EmptyHttpResponseException;
import org.appwork.utils.net.httpserver.ContentSecurityPolicy;
import org.appwork.utils.net.httpserver.ReferrerPolicy;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;
import org.appwork.utils.net.httpserver.XContentTypeOptions;
import org.appwork.utils.net.httpserver.XFrameOptions;

/**
 * Tests for HTTP server security headers functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Default security headers are present in responses</li>
 * <li>Security headers can be configured via HttpServer</li>
 * <li>Incompatible header combinations are detected and rejected</li>
 * <li>Compatible header combinations work correctly</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerSecurityHeadersTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testDefaultSecurityHeaders();
            this.testDefaultServerHeaderPresent();
            this.testCustomServerHeader();
            this.testServerHeaderDisabled();
            this.testIncompatibleFramingHeadersSameoriginNone();
            this.testIncompatibleFramingHeadersCspFirst();
            this.testCompatibleFramingHeadersDenyNone();
            this.testCompatibleFramingHeadersSameoriginSelf();

            this.testCustomCspWithAdditionalDirectives();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Default security headers should be present in responses
     *
     * This test verifies that default security headers (X-Content-Type-Options, Content-Security-Policy, Referrer-Policy) are automatically
     * added to all responses.
     */
    private void testDefaultSecurityHeaders() throws Exception {
        LogV3.info("Test: Default Security Headers");

        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        if (responseCode != 200) {
            this.logContextOnFailure(context, "Request should return 200, was: " + responseCode);
        }
        this.assertTrueWithContext(responseCode == 200, "Request should return 200, was: " + responseCode, context);

        // Verify X-Content-Type-Options header
        final String xContentTypeOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
        if (xContentTypeOptions == null) {
            this.logContextOnFailure(context, "X-Content-Type-Options header should be present");
        }
        this.assertTrueWithContext(xContentTypeOptions != null, "X-Content-Type-Options header should be present", context);
        if (xContentTypeOptions != null && !XContentTypeOptions.NOSNIFF.getValue().equalsIgnoreCase(xContentTypeOptions)) {
            this.logContextOnFailure(context, "X-Content-Type-Options should be \"" + XContentTypeOptions.NOSNIFF.getValue() + "\", was: \"" + xContentTypeOptions + "\"");
        }
        this.assertTrueWithContext(XContentTypeOptions.NOSNIFF.getValue().equalsIgnoreCase(xContentTypeOptions), "X-Content-Type-Options should be \"" + XContentTypeOptions.NOSNIFF.getValue() + "\", was: \"" + xContentTypeOptions + "\"", context);
        LogV3.info("X-Content-Type-Options header test passed: " + xContentTypeOptions);

        // Verify Content-Security-Policy header (should contain default-src 'none' and frame-ancestors 'none')
        final String contentSecurityPolicy = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
        if (contentSecurityPolicy == null) {
            this.logContextOnFailure(context, "Content-Security-Policy header should be present");
        }
        this.assertTrueWithContext(contentSecurityPolicy != null, "Content-Security-Policy header should be present", context);
        if (contentSecurityPolicy != null) {
            if (!contentSecurityPolicy.contains("default-src")) {
                this.logContextOnFailure(context, "Content-Security-Policy should contain default-src directive, was: \"" + contentSecurityPolicy + "\"");
            }
            this.assertTrueWithContext(contentSecurityPolicy.contains("default-src"), "Content-Security-Policy should contain default-src directive, was: \"" + contentSecurityPolicy + "\"", context);
            if (!contentSecurityPolicy.contains("default-src 'none'")) {
                this.logContextOnFailure(context, "Content-Security-Policy should contain default-src 'none', was: \"" + contentSecurityPolicy + "\"");
            }
            this.assertTrueWithContext(contentSecurityPolicy.contains("default-src 'none'"), "Content-Security-Policy should contain default-src 'none', was: \"" + contentSecurityPolicy + "\"", context);
            if (!contentSecurityPolicy.contains("frame-ancestors")) {
                this.logContextOnFailure(context, "Content-Security-Policy should contain frame-ancestors directive, was: \"" + contentSecurityPolicy + "\"");
            }
            this.assertTrueWithContext(contentSecurityPolicy.contains("frame-ancestors"), "Content-Security-Policy should contain frame-ancestors directive, was: \"" + contentSecurityPolicy + "\"", context);
            if (!contentSecurityPolicy.contains("frame-ancestors 'none'")) {
                this.logContextOnFailure(context, "Content-Security-Policy should contain frame-ancestors 'none', was: \"" + contentSecurityPolicy + "\"");
            }
            this.assertTrueWithContext(contentSecurityPolicy.contains("frame-ancestors 'none'"), "Content-Security-Policy should contain frame-ancestors 'none', was: \"" + contentSecurityPolicy + "\"", context);
        }
        LogV3.info("Content-Security-Policy header test passed: " + contentSecurityPolicy);

        // Verify X-Frame-Options header is NOT present (CSP frame-ancestors is used instead)
        final String xFrameOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
        if (xFrameOptions != null) {
            this.logContextOnFailure(context, "X-Frame-Options header should NOT be present when CSP frame-ancestors is used, but was: \"" + xFrameOptions + "\"");
        }
        this.assertTrueWithContext(xFrameOptions == null, "X-Frame-Options header should NOT be present when CSP frame-ancestors is used, but was: \"" + xFrameOptions + "\"", context);
        LogV3.info("X-Frame-Options header test passed: Not present (using CSP frame-ancestors instead)");

        // Verify Referrer-Policy header
        final String referrerPolicy = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
        if (referrerPolicy == null) {
            this.logContextOnFailure(context, "Referrer-Policy header should be present");
        }
        this.assertTrueWithContext(referrerPolicy != null, "Referrer-Policy header should be present", context);
        if (referrerPolicy != null && !ReferrerPolicy.NO_REFERRER.getValue().equalsIgnoreCase(referrerPolicy)) {
            this.logContextOnFailure(context, "Referrer-Policy should be \"" + ReferrerPolicy.NO_REFERRER.getValue() + "\", was: \"" + referrerPolicy + "\"");
        }
        this.assertTrueWithContext(ReferrerPolicy.NO_REFERRER.getValue().equalsIgnoreCase(referrerPolicy), "Referrer-Policy should be \"" + ReferrerPolicy.NO_REFERRER.getValue() + "\", was: \"" + referrerPolicy + "\"", context);
        LogV3.info("Referrer-Policy header test passed: " + referrerPolicy);

        LogV3.info("Default Security Headers test successful: All security headers are present with correct default values");
    }

    /**
     * Test: Default Server header should be present
     */
    private void testDefaultServerHeaderPresent() throws Exception {
        LogV3.info("Test: Default Server Header present");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        this.assertTrueWithContext(responseCode == 200, "Request should return 200, was: " + responseCode, context);

        final String serverHeader = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
        assertTrue(serverHeader != null, "Server header should be present by default");
        LogV3.info("Default Server header test passed: " + serverHeader);
    }

    /**
     * Test: Custom Server header value should be used when configured
     */
    private void testCustomServerHeader() throws Exception {
        LogV3.info("Test: Custom Server Header configured");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        final String previous = this.httpServer.getResponseServerHeader();
        try {
            final String customHeader = "AppWorkTestServer";
            this.httpServer.setResponseServerHeaderValue(customHeader);
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            this.assertTrueWithContext(responseCode == 200, "Request should return 200, was: " + responseCode, context);

            final String serverHeader = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
            assertTrue(customHeader.equals(serverHeader), "Server header should be \"" + customHeader + "\", was: \"" + serverHeader + "\"");
            LogV3.info("Custom Server header test passed: " + serverHeader);
        } finally {
            this.httpServer.setResponseServerHeaderValue(previous);
        }
    }

    /**
     * Test: Server header can be disabled
     */
    private void testServerHeaderDisabled() throws Exception {
        LogV3.info("Test: Server Header disabled");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello", "UTF-8");
        final String previous = this.httpServer.getResponseServerHeader();
        try {

            this.httpServer.setResponseServerHeaderValue(null);
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            this.assertTrueWithContext(responseCode == 200, "Request should return 200, was: " + responseCode, context);

            final String serverHeader = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
            assertTrue(serverHeader == null, "Server header should be absent when disabled, but was: " + serverHeader);
            LogV3.info("Server header disabled test passed: header not present");
        } finally {
            this.httpServer.setResponseServerHeaderValue(previous);
        }
    }

    /**
     * Test 1a: Incompatible X-Frame-Options and CSP frame-ancestors (SAMEORIGIN + 'none')
     */
    private void testIncompatibleFramingHeadersSameoriginNone() throws Exception {
        LogV3.info("Test 1a: Incompatible X-Frame-Options SAMEORIGIN + CSP frame-ancestors 'none'");
        this.teardownServer();
        this.setupServer();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            // Default CSP has frame-ancestors 'none', but we set X-Frame-Options SAMEORIGIN
            // These are incompatible, so an IllegalArgumentException is thrown on the server when headers are added to response
            final ResponseSecurityHeaders incompatibleConfig = new ResponseSecurityHeaders();
            incompatibleConfig.setXFrameOptions(XFrameOptions.SAMEORIGIN);
            this.httpServer.setResponseSecurityHeaders(incompatibleConfig);

            try {
                final RequestContext contextIncompatible = this.httpClient.get(url);
                throw new WTFException("Expected Exception!: " + contextIncompatible);
            } catch (HttpClientException e) {
                if (Exceptions.getInstanceof(e, EmptyHttpResponseException.class) != null) {
                    LogV3.info("Test 1a passed: Incompatible headers correctly rejected");
                } else {
                    throw e;
                }
            }
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test 1a alternative: Set CSP first, then try to set incompatible X-Frame-Options
     */
    private void testIncompatibleFramingHeadersCspFirst() throws Exception {
        LogV3.info("Test 1a alternative: Incompatible headers (CSP set first)");
        this.setupServer();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            final ResponseSecurityHeaders incompatibleConfig2 = new ResponseSecurityHeaders();
            final ContentSecurityPolicy cspNone = new ContentSecurityPolicy();
            cspNone.setFrameAncestors("'none'");
            cspNone.addDirective("default-src 'none'");
            incompatibleConfig2.setContentSecurityPolicy(cspNone);
            incompatibleConfig2.setXFrameOptions(XFrameOptions.SAMEORIGIN);
            this.httpServer.setResponseSecurityHeaders(incompatibleConfig2);

            try {
                this.httpClient.get(url);
            } catch (HttpClientException e) {
                if (Exceptions.getInstanceof(e, EmptyHttpResponseException.class) != null) {
                    LogV3.info("Test 1a alternative passed: Incompatible headers correctly rejected");
                } else {
                    throw e;
                }
            }
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test 1b: Compatible X-Frame-Options and CSP frame-ancestors (DENY + 'none')
     */
    private void testCompatibleFramingHeadersDenyNone() throws Exception {
        LogV3.info("Test 1b: Compatible X-Frame-Options DENY + CSP frame-ancestors 'none'");
        this.setupServer();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            final ResponseSecurityHeaders compatibleConfig = new ResponseSecurityHeaders();
            compatibleConfig.setXFrameOptions(XFrameOptions.DENY);
            // Default CSP already has frame-ancestors 'none', which is compatible with DENY
            compatibleConfig.setReferrerPolicy(ReferrerPolicy.ORIGIN);
            this.httpServer.setResponseSecurityHeaders(compatibleConfig);

            final RequestContext context1b = this.httpClient.get(url);
            final int responseCode1b = context1b.getCode();
            if (responseCode1b != 200) {
                this.logContextOnFailure(context1b, "Request should return 200, was: " + responseCode1b);
            }
            this.assertTrueWithContext(responseCode1b == 200, "Request should return 200, was: " + responseCode1b, context1b);

            // Verify X-Frame-Options is present
            final String xFrameOptions1b = context1b.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
            assertTrue(xFrameOptions1b != null, "X-Frame-Options header should be present");
            assertTrue(XFrameOptions.DENY.getValue().equalsIgnoreCase(xFrameOptions1b), "X-Frame-Options should be \"" + XFrameOptions.DENY.getValue() + "\", was: \"" + xFrameOptions1b + "\"");

            // Verify CSP DOES contain frame-ancestors when compatible with X-Frame-Options
            final String contentSecurityPolicy1b = context1b.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
            assertTrue(contentSecurityPolicy1b != null, "Content-Security-Policy header should be present");
            assertTrue(contentSecurityPolicy1b.contains("frame-ancestors 'none'"), "Content-Security-Policy should contain frame-ancestors 'none' when compatible with X-Frame-Options DENY");

            // Verify custom Referrer-Policy
            final String referrerPolicy1b = context1b.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
            assertTrue(referrerPolicy1b != null, "Referrer-Policy header should be present");
            assertTrue(ReferrerPolicy.ORIGIN.getValue().equalsIgnoreCase(referrerPolicy1b), "Referrer-Policy should be \"" + ReferrerPolicy.ORIGIN.getValue() + "\", was: \"" + referrerPolicy1b + "\"");

            LogV3.info("Test 1b passed: Compatible headers DENY + 'none' work correctly");
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test 1c: Compatible X-Frame-Options and CSP frame-ancestors (SAMEORIGIN + 'self')
     */
    private void testCompatibleFramingHeadersSameoriginSelf() throws Exception {
        LogV3.info("Test 1c: Compatible X-Frame-Options SAMEORIGIN + CSP frame-ancestors 'self'");
        this.setupServer();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            final ResponseSecurityHeaders compatibleConfig2 = new ResponseSecurityHeaders();
            compatibleConfig2.setXFrameOptions(XFrameOptions.SAMEORIGIN);
            final ContentSecurityPolicy cspSelf = new ContentSecurityPolicy();
            cspSelf.setFrameAncestors("'self'");
            cspSelf.addDirective("default-src 'none'");
            compatibleConfig2.setContentSecurityPolicy(cspSelf);
            compatibleConfig2.setReferrerPolicy(ReferrerPolicy.ORIGIN);
            this.httpServer.setResponseSecurityHeaders(compatibleConfig2);

            final RequestContext context1c = this.httpClient.get(url);
            final int responseCode1c = context1c.getCode();
            if (responseCode1c != 200) {
                this.logContextOnFailure(context1c, "Request should return 200, was: " + responseCode1c);
            }
            this.assertTrueWithContext(responseCode1c == 200, "Request should return 200, was: " + responseCode1c, context1c);

            // Verify X-Frame-Options is present
            final String xFrameOptions1c = context1c.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
            assertTrue(xFrameOptions1c != null, "X-Frame-Options header should be present");
            assertTrue(XFrameOptions.SAMEORIGIN.getValue().equalsIgnoreCase(xFrameOptions1c), "X-Frame-Options should be \"" + XFrameOptions.SAMEORIGIN.getValue() + "\", was: \"" + xFrameOptions1c + "\"");

            // Verify CSP DOES contain frame-ancestors when compatible with X-Frame-Options
            final String contentSecurityPolicy1c = context1c.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
            assertTrue(contentSecurityPolicy1c != null, "Content-Security-Policy header should be present");
            assertTrue(contentSecurityPolicy1c.contains("frame-ancestors 'self'"), "Content-Security-Policy should contain frame-ancestors 'self' when compatible with X-Frame-Options SAMEORIGIN");

            LogV3.info("Test 1c passed: Compatible headers SAMEORIGIN + 'self' work correctly");
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test 3: Custom CSP with additional directives
     */
    private void testCustomCspWithAdditionalDirectives() throws Exception {
        LogV3.info("Test 3: Custom CSP with additional directives");
        this.setupServer();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            final ResponseSecurityHeaders cspConfig = new ResponseSecurityHeaders();
            cspConfig.setXFrameOptions(null); // Use CSP frame-ancestors instead
            final ContentSecurityPolicy csp = new ContentSecurityPolicy();
            csp.setFrameAncestors("'self'");
            csp.addDirective("default-src 'self'");
            csp.addDirective("script-src 'self'");
            cspConfig.setContentSecurityPolicy(csp);
            this.httpServer.setResponseSecurityHeaders(cspConfig);

            final RequestContext context3 = this.httpClient.get(url);
            final int responseCode3 = context3.getCode();
            if (responseCode3 != 200) {
                this.logContextOnFailure(context3, "Request should return 200, was: " + responseCode3);
            }
            this.assertTrueWithContext(responseCode3 == 200, "Request should return 200, was: " + responseCode3, context3);

            // Verify CSP is present with frame-ancestors
            final String contentSecurityPolicy3 = context3.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
            assertTrue(contentSecurityPolicy3 != null, "Content-Security-Policy header should be present");
            assertTrue(contentSecurityPolicy3.contains("frame-ancestors 'self'"), "CSP should contain frame-ancestors 'self'");
            assertTrue(contentSecurityPolicy3.contains("default-src 'self'"), "CSP should contain default-src 'self'");

            // Verify X-Frame-Options is not present (using CSP instead)
            final String xFrameOptions3 = context3.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
            assertTrue(xFrameOptions3 == null, "X-Frame-Options header should not be present when using CSP frame-ancestors");

            LogV3.info("Test 3 passed: Custom CSP with additional directives works correctly");
        } finally {
            this.teardownServer();
        }
    }
}
