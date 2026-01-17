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

import java.io.ByteArrayOutputStream;
import java.net.URLEncoder;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;

/**
 * Tests for HTTP server request size limits functionality.
 * 
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 *   <li>Request header size limits are enforced</li>
 *   <li>POST body size limits are enforced</li>
 *   <li>POST processed size limits (after decompression) are enforced</li>
 *   <li>GZIP-compressed POST data is properly handled</li>
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
            this.testPostSizeLimitWithinLimit();
            this.testPostSizeLimitExceeded();
            this.testPostProcessedSizeLimitWithinLimit();
            this.testPostProcessedSizeLimitExceeded();
            this.testPostProcessedSizeLimitExceededWithGzip();
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
        LogV3.info("Request Size Limit (within limit) test successful: " + responseCode);
    }

    /**
     * Test: Request size limit - request exceeding limit should fail Uses default limits (16KB header) - tests with 20KB request
     */
    private void testRequestSizeLimitExceeded() throws Exception {
        LogV3.info("Test: Request Size Limit - Exceeded");
        // Server already has default limits (16KB header, 10MB POST)

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
        LogV3.info("Request Size Limit (exceeded) test successful: " + responseCode);
    }

    /**
     * Test: POST size limit - POST within limit should succeed Uses default limits (10MB POST) - tests with 5MB POST
     */
    private void testPostSizeLimitWithinLimit() throws Exception {
        LogV3.info("Test: POST Size Limit - Within Limit");
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Server already has default limits (16KB header, 10MB POST)

            // Create POST data that is within the limit (5MB < 10MB default)
            final StringBuilder postData = new StringBuilder();
            for (int i = 0; i < 5 * 1024 * 1024; i++) {
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
                    LogV3.info("POST Size Limit (exceeded) test successful: " + responseCode);
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
     * Test: POST processed size limit - POST within processed limit should succeed Uses custom limits (10MB POST, 5MB processed) - tests
     * with 3MB processed data
     */
    private void testPostProcessedSizeLimitWithinLimit() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Within Limit");
        // Setup server with POST body limit (10MB) and processed limit (5MB)
        this.teardownServer();
        this.setupServerWithLimits(-1, 10 * 1024 * 1024, 5 * 1024 * 1024); // No header limit, 10MB POST limit, 5MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Create POST data that is within both limits (3MB processed < 5MB processed limit, and < 10MB POST limit)
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
     * Test: POST processed size limit - POST exceeding processed limit should fail Uses custom limits (10MB POST, 2MB processed) - tests
     * with 3MB processed data
     */
    private void testPostProcessedSizeLimitExceeded() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Exceeded");
        // Setup server with POST body limit (10MB) and smaller processed limit (2MB)
        this.teardownServer();
        this.setupServerWithLimits(-1, 10 * 1024 * 1024, 2 * 1024 * 1024); // No header limit, 10MB POST limit, 2MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Create POST data that exceeds processed limit (3MB > 2MB processed limit) but is within POST body limit (3MB < 10MB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 3 * 1024 * 1024; i++) {
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
                    // POST exceeding processed limit must return 413 Request Entity Too Large
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST exceeding processed limit must return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
                    LogV3.info("POST Processed Size Limit (exceeded) test successful: " + responseCode);
                } catch (final HttpClientException e) {
                    // HttpClientException should not occur - server should send 413 response before closing
                    throw new Exception("POST Processed Size Limit test failed: Expected 413 response code but got HttpClientException: " + e.getMessage(), e);
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

    private void testPostProcessedSizeLimitExceededWithGzip() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Exceeded with GZIP Compression");
        // Setup server with POST body limit (1MB) and smaller processed limit (2MB)
        this.teardownServer();
        final int postLimit = 1 * 1024 * 1024;
        final int processLimit = 2 * 1024 * 1024;
        this.setupServerWithLimits(-1, postLimit, processLimit); // No header limit, 1MB POST limit, 2MB processed limit

        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

            try {
                // Create uncompressed data that will exceed processed limit (3MB > 2MB processed limit)
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

                // Verify compressed data is within POST limit (should be much smaller than 1MB)
                assertTrue(compressedData.length < postLimit, "Compressed data should be within POST limit");
                assertTrue(jsonData.getBytes("UTF-8").length > processLimit, "uncompressed data should exceed process limit");

                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout(50000);
                this.httpClient.setReadTimeout(100000);
                try {
                    // Send compressed data
                    final RequestContext context = this.httpClient.post(url, compressedData);
                    final int responseCode = context.getCode();
                    // POST with GZIP that exceeds processed limit after decompression must return 413 Request Entity Too Large
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "GZIP POST exceeding processed limit must return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
                    LogV3.info("POST Processed Size Limit (exceeded with GZIP) test successful: " + responseCode);
                } catch (final HttpClientException e) {
                    // HttpClientException should not occur - server should send 413 response before closing
                    throw new Exception("POST Processed Size Limit (GZIP) test failed: Expected 413 response code but got HttpClientException: " + e.getMessage(), e);
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
}

