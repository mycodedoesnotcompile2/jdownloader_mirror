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
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.zip.GZIPOutputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.serializer.Deser;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReadableBytes;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.RequestSizeLimitExceededException;

/**
 * Tests for HTTP server POST processed size limits functionality (after decompression).
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>POST processed size limits (after decompression) are enforced</li>
 * <li>GZIP-compressed POST data is properly handled</li>
 * <li>High compression ratios are handled correctly</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerPostProcessedSizeLimitsTest extends HttpServerTestBase {
    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testPostProcessedSizeLimitWithinLimit();
            this.testPostProcessedSizeLimitExceeded();
            this.testPostProcessedSizeLimitExceededWithGzip();
        } finally {
            this.teardownServer();
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
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(5), ReadableBytes.Unit.MB.toKibiBytes(10)); // No header limit, 5MB
                                                                                                                     // POST limit, 10MB
                                                                                                                     // processed limit
        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Reset server-side tracking
                this.lastServerException = null;
                this.lastRequest = null;
                this.lastResponse = null;
                // Create POST data that is within both limits (3MB < 5MB POST limit, and < 10MB processed limit)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(3));
                final String jsonData = "{\"params\":[" + Deser.toString(postData) + "]}";
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
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(5), ReadableBytes.Unit.MB.toKibiBytes(2)); // No header limit, 5MB
                                                                                                                    // POST limit, 2MB
                                                                                                                    // processed limit
        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Reset server-side tracking
                this.lastServerException = null;
                this.lastRequest = null;
                this.lastResponse = null;
                // Create uncompressed data that exceeds processed limit (3MB > 2MB processed limit)
                final String uncompressedData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(3));
                final String jsonData = "{\"params\":[" + Deser.toString(uncompressedData) + "]}";
                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                GZIPOutputStream gzipOut = null;
                try {
                    gzipOut = new GZIPOutputStream(baos);
                    gzipOut.write(jsonData.getBytes("UTF-8"));
                } finally {
                    if (gzipOut != null) {
                        try {
                            gzipOut.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
                final byte[] compressedData = baos.toByteArray();
                // Verify compressed data is within POST limit but uncompressed exceeds processed limit
                assertTrue(compressedData.length < ReadableBytes.Unit.MB.toKibiBytes(5), "Compressed data should be within POST limit (5MB)");
                assertTrue(jsonData.getBytes("UTF-8").length > ReadableBytes.Unit.MB.toKibiBytes(2), "Uncompressed data should exceed processed limit (2MB)");
                LogV3.info("Test data: Compressed=" + compressedData.length + " bytes, Uncompressed=" + jsonData.getBytes("UTF-8").length + " bytes");
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
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
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
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
     * Test: POST processed size limit with very high compression ratio POST body is very small (within 1MB POST limit) but expands to large
     * size exceeding processed limit (2MB). This tests edge case with high compression ratios.
     */
    private void testPostProcessedSizeLimitExceededWithGzip() throws Exception {
        LogV3.info("Test: POST Processed Size Limit - Exceeded with GZIP (High Compression Ratio)");
        // Setup server with small POST limit (1MB) but allow slightly larger processed data (2MB)
        // This tests the case where tiny compressed data expands to large size
        this.teardownServer();
        final int postLimit = (int) ReadableBytes.Unit.MB.toKibiBytes(1); // 1MB POST limit
        final int processLimit = (int) ReadableBytes.Unit.MB.toKibiBytes(2); // 2MB processed limit
        this.setupServerWithLimits(-1, postLimit, processLimit); // No header limit, 1MB POST limit, 2MB processed limit
        try {
            // Allow POST method for this test
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                final byte[] uncompressedData = generateRandomBytes((int) ReadableBytes.Unit.KB.toKibiBytes(1));
                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                int rawcount = 0;
                GZIPOutputStream gzipOut = null;
                try {
                    gzipOut = new GZIPOutputStream(baos);
                    while (rawcount < processLimit) {
                        rawcount += uncompressedData.length;
                        gzipOut.write(uncompressedData);
                    }
                } finally {
                    if (gzipOut != null) {
                        try {
                            gzipOut.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
                final byte[] compressedData = baos.toByteArray();
                // Verify compressed data is within POST limit (should be much smaller than 1MB due to high compression)
                assertTrue(compressedData.length < postLimit, "Compressed data should be within POST limit (1MB)");
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
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
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
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
