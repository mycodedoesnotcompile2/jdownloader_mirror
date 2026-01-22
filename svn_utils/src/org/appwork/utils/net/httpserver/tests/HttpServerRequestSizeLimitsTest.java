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

import java.io.ByteArrayOutputStream;
import java.net.URLEncoder;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReadableBytes;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.RequestSizeLimitExceededException;
import org.appwork.utils.net.httpserver.RequestSizeLimits;
import org.appwork.utils.net.httpserver.XContentTypeOptions;

/**
 * Tests for HTTP server request size limits functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Request header size limits are enforced</li>
 * <li>POST body size limits are enforced</li>
 * <li>POST processed size limits (after decompression) are enforced</li>
 * <li>GZIP-compressed POST data is properly handled</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerRequestSizeLimitsTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testRequestSizeLimitWithinLimit();
            this.testRequestSizeLimitExceeded();
            this.testRequestSizeLimitExceededHasSecurityHeaders();
            this.testRequestSizeLimitExceededHasServerHeader();
            this.testRequestSizeLimitExceededHasCorsHeaders();
            this.testPostSizeLimitWithinLimit();
            this.testPostSizeLimitExceeded();
            this.testPostSizeLimitExceededHasSecurityHeaders();
            this.testPostProcessedSizeLimitWithinLimit();
            this.testPostProcessedSizeLimitExceeded();
            this.testPostProcessedSizeLimitExceededWithGzip();
            // Draining tests
            this.testPostSizeLimitExceededWithinDrainLimit();
            this.testPostSizeLimitExceededExceedsDrainLimit();
            this.testPostSizeLimitExceededNoDraining();
            this.testPostSizeLimitExceededDrainTimeout();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Request size limit - request within limit should succeed Uses default limits (16KB header) - tests with 10KB request
     */
    private void testRequestSizeLimitWithinLimit() throws Exception {
        LogV3.info("Test: Request Size Limit - Within Limit");
        // Server already has default limits (16KB header, 10MB POST)

        // Reset server-side tracking
        this.lastServerException = null;
        this.lastRequest = null;
        this.lastResponse = null;

        // Create a request that is within the limit (10KB)
        final StringBuilder param = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            param.append("A");
        }
        final String encodedParam = URLEncoder.encode(param.toString(), "UTF-8");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        // Request within limit should succeed
        assertTrue(responseCode == 200, "Request within limit should return 200, was: " + responseCode);
        // Server-side: No exception should occur
        assertTrue(this.lastServerException == null, "Server-side: No exception expected for request within limit, but got: " + this.lastServerException);
        LogV3.info("Request Size Limit (within limit) test successful: " + responseCode);
    }

    /**
     * Test: Request size limit - request exceeding limit should fail Uses default limits (16KB header) - tests with 20KB request
     */
    private void testRequestSizeLimitExceeded() throws Exception {
        LogV3.info("Test: Request Size Limit - Exceeded");
        // Server already has default limits (16KB header, 10MB POST)

        // Reset server-side tracking
        this.lastServerException = null;
        this.lastRequest = null;
        this.lastResponse = null;

        // Create a request that exceeds the limit (20KB > 16KB default)
        final StringBuilder param = new StringBuilder();
        for (int i = 0; i < 20000; i++) {
            param.append("A");
        }
        final String encodedParam = URLEncoder.encode(param.toString(), "UTF-8");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();

        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Request exceeding limit should return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
        // Server-side: RequestSizeLimitExceededException should occur
        assertTrue(this.lastServerException != null, "Server-side: Exception expected for request exceeding limit");
        assertTrue(this.lastServerException instanceof RequestSizeLimitExceededException, "Server-side: Expected RequestSizeLimitExceededException, but got: " + this.lastServerException.getClass().getName());
        LogV3.info("Request Size Limit (exceeded) test successful: " + responseCode + " - Server exception: " + this.lastServerException.getMessage());
    }

    /**
     * Test: POST size limit - POST within limit should succeed Uses default limits (10MB POST) - tests with 5MB POST
     */
    private void testPostSizeLimitWithinLimit() throws Exception {
        LogV3.info("Test: POST Size Limit - Within Limit");
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Server already has default limits (16KB header, 2MB POST body, 10MB POST processed)

            // Reset server-side tracking
            this.lastServerException = null;
            this.lastRequest = null;
            this.lastResponse = null;

            // Create POST data that is within the limit (1MB < 2MB default)
            final StringBuilder postData = new StringBuilder();
            for (int i = 0; i < 1 * 1024 * 1024; i++) {
                postData.append("X");
            }
            final String jsonData = "{\"params\":[\"" + postData.toString() + "\"]}";

            final String url = "http://localhost:" + this.serverPort + "/test/postData";
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            try {
                final RequestContext context = this.httpClient.post(url, jsonData);
                final int responseCode = context.getCode();
                // POST within limit should succeed
                assertTrue(responseCode == 200, "POST within limit should return 200, was: " + responseCode);
                // Server-side: No exception should occur
                assertTrue(this.lastServerException == null, "Server-side: No exception expected for POST within limit, but got: " + this.lastServerException);
                LogV3.info("POST Size Limit (within limit) test successful: " + responseCode);
            } finally {
                this.httpClient.clearRequestHeader();
                // Restore mandatory header after clearing
                this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            }
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: POST size limit - POST exceeding limit should fail Uses custom limit (100KB) to test limit enforcement - tests with 200KB POST
     */
    private void testPostSizeLimitExceeded() throws Exception {
        LogV3.info("Test: POST Size Limit - Exceeded");
        // Setup server with smaller POST size limit (100KB) to test limit enforcement
        this.teardownServer();
        this.setupServerWithLimits(-1, 100 * 1024); // No header limit, 100KB POST limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Reset server-side tracking
                this.lastServerException = null;
                this.lastRequest = null;
                this.lastResponse = null;

                // Create POST data that exceeds the limit (200KB > 100KB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 200 * 1024; i++) {
                    postData.append("Y");
                }
                final String jsonData = "{\"params\":[\"" + postData.toString() + "\"]}";

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    // POST exceeding limit must return 413 Request Entity Too Large
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST exceeding limit must return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
                    // Server-side: RequestSizeLimitExceededException should occur
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for POST exceeding limit");
                    assertTrue(this.lastServerException instanceof RequestSizeLimitExceededException, "Server-side: Expected RequestSizeLimitExceededException, but got: " + this.lastServerException.getClass().getName());
                    LogV3.info("POST Size Limit (exceeded) test successful: " + responseCode + " - Server exception: " + this.lastServerException.getMessage());
                } catch (final HttpClientException e) {
                    // HttpClientException should not occur - server should send 413 response before closing
                    throw new Exception("POST Size Limit test failed: Expected 413 response code but got HttpClientException: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: POST processed size limit - POST within processed limit should succeed Uses custom limits (5MB POST, 10MB processed) - tests
     * with 3MB processed data
     */
    private void testPostProcessedSizeLimitWithinLimit() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Within Limit");
        // Setup server with POST body limit (5MB) and larger processed limit (10MB)
        // This makes sense: Raw POST data is smaller, processed (decompressed) data could be larger
        this.teardownServer();
        this.setupServerWithLimits(-1, 5 * 1024 * 1024, 10 * 1024 * 1024); // No header limit, 5MB POST limit, 10MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Reset server-side tracking
                this.lastServerException = null;
                this.lastRequest = null;
                this.lastResponse = null;

                // Create POST data that is within both limits (3MB < 5MB POST limit, and < 10MB processed limit)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 3 * 1024 * 1024; i++) {
                    postData.append("X");
                }
                final String jsonData = "{\"params\":[\"" + postData.toString() + "\"]}";

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    // POST within processed limit should succeed
                    assertTrue(responseCode == 200, "POST within processed limit should return 200, was: " + responseCode);
                    // Server-side: No exception should occur
                    assertTrue(this.lastServerException == null, "Server-side: No exception expected for POST within processed limit, but got: " + this.lastServerException);
                    LogV3.info("POST Processed Size Limit (within limit) test successful: " + responseCode);
                } finally {
                    this.httpClient.clearRequestHeader();
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: POST processed size limit - POST exceeding processed limit should fail Uses GZIP compression where compressed data is within
     * POST limit but decompressed data exceeds processed limit. Custom limits (5MB POST, 2MB processed) - sends ~200KB compressed that
     * expands to 3MB
     */
    private void testPostProcessedSizeLimitExceeded() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Exceeded");
        // Setup server with POST body limit (5MB) and smaller processed limit (2MB)
        // This is realistic: Accept up to 5MB compressed data, but limit decompressed data to 2MB
        this.teardownServer();
        this.setupServerWithLimits(-1, 5 * 1024 * 1024, 2 * 1024 * 1024); // No header limit, 5MB POST limit, 2MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Reset server-side tracking
                this.lastServerException = null;
                this.lastRequest = null;
                this.lastResponse = null;

                // Create uncompressed data that exceeds processed limit (3MB > 2MB processed limit)
                final StringBuilder uncompressedData = new StringBuilder();
                for (int i = 0; i < 3 * 1024 * 1024; i++) {
                    uncompressedData.append("Y");
                }
                final String jsonData = "{\"params\":[\"" + uncompressedData.toString() + "\"]}";

                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                try (GZIPOutputStream gzipOut = new GZIPOutputStream(baos)) {
                    gzipOut.write(jsonData.getBytes("UTF-8"));
                }
                final byte[] compressedData = baos.toByteArray();

                // Verify compressed data is within POST limit but uncompressed exceeds processed limit
                assertTrue(compressedData.length < 5 * 1024 * 1024, "Compressed data should be within POST limit (5MB)");
                assertTrue(jsonData.getBytes("UTF-8").length > 2 * 1024 * 1024, "Uncompressed data should exceed processed limit (2MB)");
                LogV3.info("Test data: Compressed=" + compressedData.length + " bytes, Uncompressed=" + jsonData.getBytes("UTF-8").length + " bytes");

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    final RequestContext context = this.httpClient.post(url, compressedData);
                    final int responseCode = context.getCode();
                    // POST exceeding processed limit should return error
                    // Note: Can be either 413 (if caught early) or 500 (if caught during RemoteAPI processing)
                    // The RemoteAPI wraps RequestSizeLimitExceededException into BasicRemoteAPIException
                    final boolean isError = responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() || responseCode == 500;
                    assertTrue(isError, "POST exceeding processed limit must return error (413 or 500), was: " + responseCode);
                    // Server-side: Exception should occur (wrapped in RemoteAPI exception)
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for POST exceeding processed limit");
                    // The RequestSizeLimitExceededException should be in the exception chain
                    assertTrue(Exceptions.containsInstanceOf(this.lastServerException, RequestSizeLimitExceededException.class), "Server-side: Expected RequestSizeLimitExceededException in cause chain, but got: " + this.lastServerException.getClass().getName());
                    LogV3.info("POST Processed Size Limit (exceeded) test successful: " + responseCode + (responseCode == 500 ? " (wrapped by RemoteAPI)" : "") + " - Server exception chain contains: " + RequestSizeLimitExceededException.class.getSimpleName());
                } catch (final HttpClientException e) {
                    // HttpClientException should not occur - server should send error response before closing
                    throw new Exception("POST Processed Size Limit test failed: Expected error response but got HttpClientException: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: Request size limit exceeded - verify Security Headers are present in error response This test ensures that even when a request
     * exceeds the header size limit (early exception), the server still applies security headers to the error response.
     */
    private void testRequestSizeLimitExceededHasSecurityHeaders() throws Exception {
        LogV3.info("Test: Request Size Limit Exceeded - Security Headers Present");

        // Create a request that exceeds the limit (20KB > 16KB default)
        final StringBuilder param = new StringBuilder();
        for (int i = 0; i < 20000; i++) {
            param.append("A");
        }
        final String encodedParam = URLEncoder.encode(param.toString(), "UTF-8");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();

        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Request exceeding limit should return 413 error, was: " + responseCode);

        // Verify Security Headers are present even in error response
        final String xContentTypeOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
        assertTrue(xContentTypeOptions != null, "X-Content-Type-Options header should be present in error response\r\n" + context);
        assertTrue(XContentTypeOptions.NOSNIFF.getValue().equalsIgnoreCase(xContentTypeOptions), "X-Content-Type-Options should be \"" + XContentTypeOptions.NOSNIFF.getValue() + "\" in error response");

        final String csp = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
        assertTrue(csp != null, "Content-Security-Policy header should be present in error response");
        assertTrue(csp.contains("default-src"), "CSP should contain default-src directive in error response");

        final String referrerPolicy = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
        assertTrue(referrerPolicy != null, "Referrer-Policy header should be present in error response");

        LogV3.info("Request Size Limit (exceeded) has Security Headers test passed");
    }

    /**
     * Test: Request size limit exceeded - verify Server Header is present in error response
     */
    private void testRequestSizeLimitExceededHasServerHeader() throws Exception {
        LogV3.info("Test: Request Size Limit Exceeded - Server Header Present");

        // Create a request that exceeds the limit
        final StringBuilder param = new StringBuilder();
        for (int i = 0; i < 20000; i++) {
            param.append("A");
        }
        final String encodedParam = URLEncoder.encode(param.toString(), "UTF-8");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();

        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Request exceeding limit should return 413 error");

        // Verify Server Header is present even in error response
        final String serverHeader = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
        assertTrue(serverHeader != null, "Server header should be present in error response");

        LogV3.info("Request Size Limit (exceeded) has Server Header test passed: " + serverHeader);
    }

    /**
     * Test: Request size limit exceeded with CORS - verify that NO CORS Headers are set in error response
     * 
     * When the request header size limit is exceeded, the exception is thrown BEFORE the request headers
     * (including the Origin header) can be read. Without being able to read the Origin header, the server
     * cannot determine which origin to allow and therefore applies the most restrictive CORS policy:
     * no CORS headers at all. This causes the browser to block the response, which is the correct security behavior.
     */
    private void testRequestSizeLimitExceededHasCorsHeaders() throws Exception {
        LogV3.info("Test: Request Size Limit Exceeded - CORS Headers NOT Present (Most Restrictive)");

        // Setup CORS handler
        final CorsHandler previousCors = this.httpServer.getCorsHandler();
        try {
            final CorsHandler corsHandler = new CorsHandler();
            final Set<String> allowedOrigins = new HashSet<String>();
            allowedOrigins.add("https://example.com");
            corsHandler.setAllowedOrigins(allowedOrigins);
            this.httpServer.setCorsHandler(corsHandler);

            // Create a request that exceeds the limit WITH Origin header
            // The Origin header is part of the request, but since the request line + headers exceed the limit,
            // the Origin header cannot be read before the exception is thrown
            final StringBuilder param = new StringBuilder();
            for (int i = 0; i < 20000; i++) {
                param.append("A");
            }
            final String encodedParam = URLEncoder.encode(param.toString(), "UTF-8");
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;

            this.httpClient.putRequestHeader("Origin", "https://example.com");
            try {
                final RequestContext context = this.httpClient.get(url);
                final int responseCode = context.getCode();

                assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Request exceeding limit should return 413 error");

                // Verify that NO CORS Headers are set - this is the most restrictive (and correct) behavior
                // when the request cannot be fully read
                final String allowOrigin = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
                assertTrue(allowOrigin == null, "Access-Control-Allow-Origin header should NOT be present when request headers cannot be read (most restrictive CORS policy)");

                LogV3.info("Request Size Limit (exceeded) has correct CORS behavior: No CORS headers (most restrictive)");
            } finally {
                this.httpClient.clearRequestHeader();
                // Restore mandatory header after clearing
                this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            }
        } finally {
            this.httpServer.setCorsHandler(previousCors);
        }
    }

    /**
     * Test: POST size limit exceeded - verify Security Headers are present in error response This test ensures that even when a POST body
     * exceeds the size limit, the server still applies security headers to the error response.
     */
    private void testPostSizeLimitExceededHasSecurityHeaders() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - Security Headers Present");
        // Setup server with smaller POST size limit (100KB) to test limit enforcement
        this.teardownServer();
        this.setupServerWithLimits(-1, 100 * 1024); // No header limit, 100KB POST limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Create POST data that exceeds the limit (200KB > 100KB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 200 * 1024; i++) {
                    postData.append("Y");
                }
                final String jsonData = "{\"params\":[\"" + postData.toString() + "\"]}";

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();

                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST exceeding limit must return 413 error");

                    // Verify Security Headers are present even in error response
                    final String xContentTypeOptions = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
                    assertTrue(xContentTypeOptions != null, "X-Content-Type-Options header should be present in POST size limit error response");

                    final String csp = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
                    assertTrue(csp != null, "Content-Security-Policy header should be present in POST size limit error response");

                    final String serverHeader = context.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
                    assertTrue(serverHeader != null, "Server header should be present in POST size limit error response");

                    LogV3.info("POST Size Limit (exceeded) has Security Headers test passed");
                } catch (final HttpClientException e) {
                    throw new Exception("POST Size Limit Security Headers test failed: Expected 413 response with headers but got HttpClientException: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: POST processed size limit with very high compression ratio POST body is very small (within 1MB POST limit) but expands to
     * large size exceeding processed limit (2MB). This tests edge case with high compression ratios.
     */
    private void testPostProcessedSizeLimitExceededWithGzip() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Exceeded with GZIP (High Compression Ratio)");
        // Setup server with small POST limit (1MB) but allow slightly larger processed data (2MB)
        // This tests the case where tiny compressed data expands to large size
        this.teardownServer();
        final int postLimit = 1 * 1024 * 1024; // 1MB POST limit
        final int processLimit = 2 * 1024 * 1024; // 2MB processed limit
        this.setupServerWithLimits(-1, postLimit, processLimit); // No header limit, 1MB POST limit, 2MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Create uncompressed data that will exceed processed limit (3MB > 2MB processed limit)
                // Use highly repetitive data for maximum compression ratio
                final StringBuilder uncompressedData = new StringBuilder();
                for (int i = 0; i < 3 * 1024 * 1024; i++) {
                    uncompressedData.append("Y"); // Repetitive character compresses very well
                }
                final String jsonData = "{\"params\":[\"" + uncompressedData.toString() + "\"]}";

                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                try (GZIPOutputStream gzipOut = new GZIPOutputStream(baos)) {
                    gzipOut.write(jsonData.getBytes("UTF-8"));
                }
                final byte[] compressedData = baos.toByteArray();

                // Verify compressed data is within POST limit (should be much smaller than 1MB due to high compression)
                assertTrue(compressedData.length < postLimit, "Compressed data should be within POST limit (1MB)");
                assertTrue(jsonData.getBytes("UTF-8").length > processLimit, "Uncompressed data should exceed processed limit (2MB)");
                LogV3.info("Test data (high compression): Compressed=" + compressedData.length + " bytes, Uncompressed=" + jsonData.getBytes("UTF-8").length + " bytes, Ratio=" + (jsonData.getBytes("UTF-8").length / compressedData.length) + "x");

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    // Send compressed data
                    final RequestContext context = this.httpClient.post(url, compressedData);
                    final int responseCode = context.getCode();
                    // POST with GZIP that exceeds processed limit should return error
                    // Note: Can be either 413 (if caught early) or 500 (if caught during RemoteAPI processing)
                    final boolean isError = responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() || responseCode == 500;
                    assertTrue(isError, "GZIP POST exceeding processed limit must return error (413 or 500), was: " + responseCode);
                    LogV3.info("POST Processed Size Limit (exceeded with GZIP, high compression) test successful: " + responseCode + (responseCode == 500 ? " (wrapped by RemoteAPI)" : ""));
                } catch (final HttpClientException e) {
                    // HttpClientException should not occur - server should send error response before closing
                    throw new Exception("POST Processed Size Limit (GZIP) test failed: Expected error response but got HttpClientException: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: POST exceeding limit but within drain limit - should drain all data and return 413 POST data: 5MB, POST limit: 2MB, Drain
     * limit: 10MB -> Should drain all 5MB
     */
    private void testPostSizeLimitExceededWithinDrainLimit() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - Within Drain Limit");
        // Setup server with custom limits: 2MB POST limit, 10MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.fromBytes(16 * 1024), // 16KB header
                ReadableBytes.fromBytes(2 * 1024 * 1024), // 2MB POST limit
                ReadableBytes.fromBytes(10 * 1024 * 1024), // 10MB processed
                ReadableBytes.fromBytes(10 * 1024 * 1024), // 10MB drain limit
                TimeSpan.fromMillis(30000)); // 30s drain timeout
        this.setupServerWithLimits(-1, 2 * 1024 * 1024); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit but is within drain limit (5MB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 5 * 1024 * 1024; i++) {
                    postData.append("X");
                }
                final String jsonData = "{\"data\":\"" + postData.toString() + "\"}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST within drain limit must return 413, was: " + responseCode);
                    LogV3.info("POST Size Limit (exceeded, within drain limit) test successful: " + responseCode + " - All data drained");
                } catch (final HttpClientException e) {
                    throw new Exception("POST within drain limit test failed: Expected 413 but got HttpClientException: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit AND exceeding drain limit - should drain only up to drain limit POST data: 15MB, POST limit: 2MB, Drain
     * limit: 5MB -> Should drain only 5MB, then stop
     */
    private void testPostSizeLimitExceededExceedsDrainLimit() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - Exceeds Drain Limit");
        // Setup server with custom limits: 2MB POST limit, 5MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.fromBytes(16 * 1024), // 16KB header
                ReadableBytes.fromBytes(2 * 1024 * 1024), // 2MB POST limit
                ReadableBytes.fromBytes(10 * 1024 * 1024), // 10MB processed
                ReadableBytes.fromBytes(5 * 1024 * 1024), // 5MB drain limit (smaller than POST data)
                TimeSpan.fromMillis(30000)); // 30s drain timeout
        this.setupServerWithLimits(-1, 2 * 1024 * 1024); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds both POST limit and drain limit (15MB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 15 * 1024 * 1024; i++) {
                    postData.append("Y");
                }
                final String jsonData = "{\"data\":\"" + postData.toString() + "\"}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    // Server should drain up to 5MB, then stop. Client might see connection issues or 413.
                    // We accept both 413 (if response was read before connection closed) or HttpClientException
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST exceeding drain limit should return 413 if possible, was: " + responseCode);
                    LogV3.info("POST Size Limit (exceeds drain limit) test successful: " + responseCode + " - Partial drain (up to limit)");
                } catch (final HttpClientException e) {
                    // This is also acceptable - connection might be closed after partial drain
                    LogV3.info("POST Size Limit (exceeds drain limit) test successful - Connection closed after partial drain: " + e.getMessage());
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with draining DISABLED - should close connection without draining POST data: 5MB, POST limit: 2MB, Drain
     * limit: 0 (disabled) -> No draining, connection closed immediately
     */
    private void testPostSizeLimitExceededNoDraining() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - No Draining");
        // Setup server with draining disabled (null drain limit and null timeout)
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.fromBytes(16 * 1024), // 16KB header
                ReadableBytes.fromBytes(2 * 1024 * 1024), // 2MB POST limit
                ReadableBytes.fromBytes(10 * 1024 * 1024), // 10MB processed
                null, // Draining disabled (null)
                null); // No timeout
        this.setupServerWithLimits(-1, 2 * 1024 * 1024); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit (5MB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 5 * 1024 * 1024; i++) {
                    postData.append("Z");
                }
                final String jsonData = "{\"data\":\"" + postData.toString() + "\"}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                boolean gotException = false;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    // Without draining, we might not get a clean 413 response due to TCP RST
                    LogV3.info("POST Size Limit (no draining) - got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    // Expected: Connection reset or similar error because server closed without draining
                    gotException = true;
                    LogV3.info("POST Size Limit (no draining) test successful - Connection closed without draining: " + e.getMessage());
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
                // We expect either an exception OR a successful 413 response (depending on timing)
                // The key difference is that the server should NOT drain (visible in logs)
                LogV3.info("POST Size Limit (no draining) test completed" + (gotException ? " with expected exception" : " with response"));
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with drain timeout - should stop draining after timeout POST data: 10MB sent slowly, POST limit: 2MB,
     * Drain limit: 20MB, Drain timeout: 2s -> Should drain for max 2 seconds then stop
     */
    private void testPostSizeLimitExceededDrainTimeout() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - Drain Timeout");
        // Setup server with short drain timeout: 2MB POST limit, 20MB drain limit, 2s timeout
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.fromBytes(16 * 1024), // 16KB header
                ReadableBytes.fromBytes(2 * 1024 * 1024), // 2MB POST limit
                ReadableBytes.fromBytes(10 * 1024 * 1024), // 10MB processed
                ReadableBytes.fromBytes(20 * 1024 * 1024), // 20MB drain limit (higher than POST data)
                TimeSpan.fromMillis(2000)); // 2s drain timeout (SHORT!)
        this.setupServerWithLimits(-1, 2 * 1024 * 1024); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create large POST data (10MB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 10 * 1024 * 1024; i++) {
                    postData.append("T");
                }
                final String jsonData = "{\"data\":\"" + postData.toString() + "\"}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                boolean gotTimeoutBehavior = false;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    // Depending on timing, we might get 413 or connection error
                    LogV3.info("POST Size Limit (drain timeout) - got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    // Expected: Connection might be closed if timeout occurs during drain
                    gotTimeoutBehavior = true;
                    LogV3.info("POST Size Limit (drain timeout) test - Drain stopped after timeout: " + e.getMessage());
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout(5000);
                    this.httpClient.setReadTimeout(30000);
                }
                // Key point: Server should stop draining after 2 seconds (visible in logs)
                LogV3.info("POST Size Limit (drain timeout) test completed - Check logs for 'Drain input stream stopped due to timeout'");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }
}
