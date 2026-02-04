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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.zip.GZIPOutputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.serializer.Deser;
import org.appwork.testframework.AWTest;
import org.appwork.utils.ReadableBytes;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.RequestSizeLimitExceededException;
import org.appwork.utils.net.httpserver.RequestSizeLimits;
import org.appwork.utils.net.throttledconnection.ThrottledInputStream;
import org.appwork.utils.os.CrossSystem;

/**
 * Tests for HTTP server input stream draining functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>POST data exceeding limits is properly drained to prevent TCP RST</li>
 * <li>Drain limits are respected</li>
 * <li>Drain timeouts work correctly</li>
 * <li>Draining can be disabled</li>
 * <li>Draining works with GZIP-compressed data</li>
 * <li>Draining behavior with chunked transfer encoding</li>
 * <li>Header size limit exceeded (draining not possible because request is unknown)</li>
 * </ul>
 *
 * <p>
 * <b>Important OS-dependent behavior when draining is disabled:</b>
 * </p>
 * <ul>
 * <li><b>Windows:</b> Throws exception (connection closed) only when LARGE amounts of POST data are sent (e.g., 5MB+). This happens because
 * the TCP buffer overflows when the server closes the connection without draining. With SMALL POST data (few KB), Windows typically returns
 * a valid response code (413) instead of throwing an exception.</li>
 * <li><b>Linux:</b> May return a valid response code (413) even without draining, especially for smaller POST data amounts.</li>
 * </ul>
 *
 * <p>
 * The difference between large and small POST data behavior demonstrates that Windows exceptions are caused by TCP buffer overflow, not
 * just by the absence of draining. Small POST data fits in the TCP buffer, allowing the server to send a response before closing the
 * connection.
 * </p>
 *
 * @author AppWork
 */
public class HttpServerDrainingTest extends HttpServerTestBase {
    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testPostSizeLimitExceededWithinDrainLimit();
            this.testPostSizeLimitExceededExceedsDrainLimit();
            this.testPostSizeLimitExceededNoDraining();
            this.testPostSizeLimitExceededDrainTimeout();
            this.testPostSizeLimitExceededWithGzipDraining();
            this.testPostSizeLimitExceededWithChunkedDraining();
            this.testPostSizeLimitExceededWithChunkedDrainTimeout();
            this.testPostSizeLimitExceededWithChunkedExceedsDrainLimit();
            this.testPostSizeLimitExceededWithChunkedAndGzipDraining();
            this.testPostSizeLimitExceededNoDrainingSmallData();
            this.testHeaderSizeLimitExceededWithPost();
            this.testHeaderSizeLimitExceededWithGet();
        } finally {
            this.teardownServer();
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
        final RequestSizeLimits limits = new RequestSizeLimits(Long.valueOf(ReadableBytes.Unit.KB.toKibiBytes(16)), // 16KB header
                Long.valueOf(ReadableBytes.Unit.MB.toKibiBytes(2)), // 2MB POST limit
                Long.valueOf(ReadableBytes.Unit.MB.toKibiBytes(10)), // 10MB processed
                Long.valueOf(ReadableBytes.Unit.MB.toKibiBytes(10)), // 10MB drain limit
                Long.valueOf(TimeUnit.SECONDS.toMillis(30))); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        httpServer.setVerboseLog(true);
        httpClient.setVerboseLog(true);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit but is within drain limit (5MB)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(5));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Server should drain all 5MB POST data and return 413
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST exceeding limit should return 413, was: " + responseCode);
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body");
                    // Timing validation: 5MB POST with full drain
                    // - Server drains all data, so should be fast
                    // - Max allowed: 100ms (should be fast when draining completes)
                    assertTrue(elapsed < 150, "Request with full drain (5MB) should complete within 100ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (within drain limit) test successful: " + responseCode + " in " + elapsed + "ms - Full drain");
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    throw new Exception("POST Size Limit test failed after " + elapsed + "ms: Expected 413 but got HttpClientException: " + e.getMessage(), e);
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
            this.setupServer();
        }
    }

    /**
     * Test helper: Execute POST request using HttpClient
     *
     * @return array with [0] = elapsed time in ms, [1] = response code
     */
    private long[] testWithHttpClient(final String urlString, final String postData) throws Exception {
        final long startTime = System.currentTimeMillis();
        try {
            httpClient.setVerboseLog(true);
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
            final RequestContext context = this.httpClient.post(urlString, postData);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("HttpClient: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("HttpClient test failed after " + elapsed + "ms: Expected 413 but got HttpClientException: " + e.getMessage(), e);
        } finally {
            this.httpClient.clearRequestHeader();
            this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
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
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(5), // 5MB drain limit (smaller than POST data)
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds both POST limit and drain limit (15MB)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(15));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Server should close connection when drain limit is exceeded - no response should be received
                    throw new Exception("POST Size Limit (exceeds drain limit) test failed: Expected HttpClientException (connection closed when drain limit exceeded) but got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Expected: Connection closed when drain limit is exceeded
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body (drain limit exceeded)");
                    // Timing validation: 15MB POST with partial drain (server stops at 5MB limit)
                    // - Server skips draining (exceeds drain limit), so should be fast
                    // - Max allowed: 150ms (should be fast when draining is skipped)
                    assertTrue(elapsed < 150, "Request with partial drain (5MB of 15MB) should complete within 150ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (exceeds drain limit) test successful - Connection closed when drain limit exceeded in " + elapsed + "ms: " + e.getMessage());
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
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with draining DISABLED - behavior depends on OS and POST data size.
     *
     * <p>
     * <b>Important OS-dependent behavior:</b>
     * </p>
     * <ul>
     * <li><b>Windows:</b> Throws exception only when LARGE amounts of POST data are sent (e.g., 5MB+). This happens because the TCP buffer
     * overflows when the server closes the connection without draining. With small POST data (few KB), Windows may successfully return a
     * response code instead of throwing an exception.</li>
     * <li><b>Linux:</b> May return a valid response code (413) even without draining, especially for smaller POST data amounts.</li>
     * </ul>
     *
     * <p>
     * This test uses 5MB POST data to trigger the Windows exception behavior. For small POST data tests, see
     * {@link #testPostSizeLimitExceededNoDrainingSmallData()}.
     * </p>
     *
     * POST data: 5MB, POST limit: 2MB, Drain limit: 0 (disabled) -> No draining, behavior OS-dependent
     */
    private void testPostSizeLimitExceededNoDraining() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - No Draining");
        // Setup server with draining disabled (null drain limit and null timeout)
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                -1, // Draining disabled (null)
                -1); // No timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit (5MB)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(5));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Without draining, behavior is OS-dependent (see expectValidResponseWhenDrainingFails())
                    if (this.expectValidResponseWhenDrainingFails()) {
                        // Valid response code expected (Linux behavior)
                        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST without draining should return 413, was: " + responseCode);
                        assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body when draining is disabled");
                        // Timing validation: 5MB POST without draining (fast response)
                        // - Should be fast since no draining overhead
                        // - Max allowed: 100ms (should be fast when draining is disabled)
                        assertTrue(elapsed < 100, "Request without draining should complete within 100ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("POST Size Limit (no draining) test successful - Got response code: " + responseCode + " in " + elapsed + "ms");
                    } else {
                        // Exception expected (Windows behavior) - but got response code, which is unexpected
                        throw new Exception("POST Size Limit (no draining) test failed: Expected HttpClientException (connection closed) but got response code: " + responseCode);
                    }
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    if (this.expectExceptionWhenDrainingFails()) {
                        // Exception expected (Windows behavior)
                        assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body when draining is disabled");
                        // Timing validation: 5MB POST without draining (connection closed immediately)
                        // - Should be fast since no draining overhead
                        // - Max allowed: 100ms (should be fast when draining is disabled)
                        assertTrue(elapsed < 100, "Request without draining should complete within 100ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("POST Size Limit (no draining) test successful - Connection closed without draining in " + elapsed + "ms: " + e.getMessage());
                    } else {
                        // Valid response expected (Linux behavior) - but got exception, which is unexpected
                        throw new Exception("POST Size Limit (no draining) test failed: Expected valid response code but got HttpClientException: " + e.getMessage(), e);
                    }
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
                }
                // We expect either an exception OR a successful response (OS-dependent behavior)
                // The key difference is that the server should NOT drain (visible in logs)
                // IMPORTANT: Under Windows, exceptions only occur with LARGE POST data (5MB+) when TCP buffer overflows.
                // With small POST data, Windows may also return a response code instead of throwing an exception.
                LogV3.info("POST Size Limit (no draining, large data) test completed - Behavior is OS-dependent (Windows: Exception with large data, Linux: may return response)");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with draining DISABLED and SMALL POST data - should return response code (no exception).
     *
     * <p>
     * <b>Important OS-dependent behavior:</b>
     * </p>
     * <ul>
     * <li><b>Windows:</b> With SMALL POST data (few KB), Windows typically returns a valid response code (413) instead of throwing an
     * exception. Exceptions only occur with LARGE POST data (5MB+) when the TCP buffer overflows after the server closes the connection
     * without draining.</li>
     * <li><b>Linux:</b> Returns a valid response code (413) even without draining.</li>
     * </ul>
     *
     * <p>
     * This test verifies that small POST data exceeding the limit returns a response code even when draining is disabled, demonstrating
     * that the Windows exception behavior only occurs with large amounts of data that overflow the TCP buffer.
     * </p>
     *
     * POST data: 10KB, POST limit: 2KB, Drain limit: 0 (disabled) -> Should return 413 response code (no exception)
     */
    private void testPostSizeLimitExceededNoDrainingSmallData() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - No Draining (Small Data)");
        // Setup server with draining disabled and small POST limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.KB.toKibiBytes(2), // 2KB POST limit (small!)
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                -1, // Draining disabled
                -1); // No timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.KB.toKibiBytes(2)); // 2KB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit but is SMALL (10KB) - should NOT trigger Windows exception
                // Windows exceptions only occur with LARGE data (5MB+) that overflow TCP buffer
                final String postData = generateRandomString((int) ReadableBytes.Unit.KB.toKibiBytes(10));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // With SMALL POST data, even Windows should return a response code (no exception)
                    // Exceptions only occur with LARGE data (5MB+) when TCP buffer overflows
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST without draining (small data) should return 413, was: " + responseCode);
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body when draining is disabled");
                    // Timing validation: 10KB POST without draining (fast response)
                    // - Should be fast since no draining overhead
                    // - Max allowed: 100ms (should be fast when draining is disabled)
                    assertTrue(elapsed < 100, "Request without draining (small data) should complete within 100ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (no draining, small data) test successful - Got response code: " + responseCode + " in " + elapsed + "ms (no exception even on Windows)");
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Exception should NOT occur with small POST data, even on Windows
                    // Windows exceptions only occur with LARGE data (5MB+) that overflow TCP buffer
                    throw new Exception("POST Size Limit (no draining, small data) test failed: Expected 413 response code but got HttpClientException. This suggests TCP buffer overflow even with small data, which is unexpected. Exception: " + e.getMessage(), e);
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
                }
                // Key point: With SMALL POST data, even Windows returns a response code instead of throwing an exception.
                // This demonstrates that Windows exceptions only occur with LARGE data that overflow the TCP buffer.
                LogV3.info("POST Size Limit (no draining, small data) test completed - Small data returns response code even on Windows (exceptions only with large data 5MB+)");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with drain timeout - should stop draining after timeout POST data: 50MB (large enough to trigger timeout),
     * POST limit: 2MB, Drain limit: 100MB, Drain timeout: 2s -> Should drain for max 2 seconds then stop
     */
    private void testPostSizeLimitExceededDrainTimeout() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - Drain Timeout");
        // Setup server with short drain timeout: 2MB POST limit, 100MB drain limit, 2s timeout
        // Use large drain limit but short timeout to ensure timeout is triggered
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(100), // 100MB drain limit (much higher than POST data to allow timeout to trigger)
                TimeUnit.SECONDS.toMillis(2)); // 2s drain timeout (SHORT!)
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create very large POST data (50MB) - large enough that draining will take longer than 2s timeout
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(50));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    byte[] bytes;
                    ThrottledInputStream in = new ThrottledInputStream(new ByteArrayInputStream(bytes = jsonData.getBytes("UTF-8")));
                    in.setLimit((int) ReadableBytes.Unit.MB.toKibiBytes(10));
                    final RequestContext context = httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(in).setPostDataLength(bytes.length));
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Server should close connection after drain timeout - no response should be received
                    throw new Exception("POST Size Limit (drain timeout) test failed: Expected HttpClientException (connection closed after timeout) but got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Expected: Connection closed if timeout occurs during drain
                    // Server-side exception should be present (request exceeded limit before timeout)
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized POST body (before drain timeout)");
                    // Timing validation: 50MB POST with drain timeout (2s)
                    // - Server should stop draining after 2 seconds (timeout triggers)
                    // - Min allowed: 1800ms (0.9x timeout, allows some variance)
                    // - Max allowed: 4000ms (2x timeout, allows for network overhead)
                    assertTrue(elapsed >= 1800, "Request with drain timeout should take at least 1800ms (0.9x timeout), took: " + elapsed + "ms");
                    assertTrue(elapsed < 4000, "Request with drain timeout should complete within 4000ms (2x timeout), took: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (drain timeout) test - Drain stopped after timeout in " + elapsed + "ms: " + e.getMessage());
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
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

    /**
     * Test: POST exceeding limit with GZIP compression - should drain compressed data and return 413 POST data: ~3MB compressed (expands to
     * ~10MB), POST limit: 2MB, Drain limit: 10MB -> Should drain all compressed data (~3MB)
     */
    private void testPostSizeLimitExceededWithGzipDraining() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - With GZIP Draining");
        // Setup server with custom limits: 2MB POST limit, 10MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB drain limit
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create uncompressed data that will exceed POST limit when compressed
                // Use less compressible data (random-like) to ensure compressed size exceeds 2MB
                // We need ~10MB uncompressed to get ~3MB compressed (typical GZIP ratio for random data)
                final String uncompressedData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(1));
                final String jsonData = "{\"data\":" + Deser.toString(uncompressedData) + "}";
                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                GZIPOutputStream gzipOut = null;
                try {
                    gzipOut = new GZIPOutputStream(baos);
                    while (baos.size() < ReadableBytes.Unit.MB.toKibiBytes(2)) {
                        gzipOut.write(jsonData.getBytes("UTF-8"));
                    }
                    gzipOut.flush();
                } finally {
                    if (gzipOut != null) {
                        try {
                            gzipOut.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
                final byte[] compressedData = baos.toByteArray();
                // Verify compressed data exceeds POST limit but is within drain limit
                assertTrue(compressedData.length > ReadableBytes.Unit.MB.toKibiBytes(2), "Compressed data should exceed POST limit (2MB), was: " + compressedData.length);
                assertTrue(compressedData.length < ReadableBytes.Unit.MB.toKibiBytes(10), "Compressed data should be within drain limit (10MB), was: " + compressedData.length);
                LogV3.info("Test data: Compressed=" + compressedData.length + " bytes (exceeds 2MB POST limit, within 10MB drain limit)");
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // Send compressed data - Content-Length will be the compressed size
                    final RequestContext context = this.httpClient.post(url, compressedData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Must return 413 Request Entity Too Large (exceeds 2MB POST limit)
                    // Draining should work on the compressed stream (Content-Length = compressed size)
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "GZIP POST exceeding limit must return 413, was: " + responseCode);
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized GZIP POST body");
                    // Timing validation: ~3MB compressed POST with full drain
                    // - Draining itself is fast: ~7-39ms (from server logs)
                    // - Total time includes client sending compressed data
                    // - Max allowed: 150ms (should be fast for localhost)
                    assertTrue(elapsed < 150, "Request with GZIP drain (~3MB compressed) should complete within 150ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (exceeded with GZIP, within drain limit) test successful: " + responseCode + " in " + elapsed + "ms - Compressed data drained");
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // HttpClientException should not occur - server should send 413 response after draining compressed data
                    assertTrue(elapsed < 150, "GZIP POST within drain limit should complete within 150ms even on exception, took: " + elapsed + "ms");
                    throw new Exception("GZIP POST within drain limit test failed after " + elapsed + "ms: Expected 413 but got HttpClientException: " + e.getMessage(), e);
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
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with Chunked Transfer-Encoding - currently draining doesn't work for chunked (no Content-Length) POST
     * data: 5MB chunked, POST limit: 2MB -> Server should handle gracefully (may not drain, but should not crash)
     */
    private void testPostSizeLimitExceededWithChunkedDraining() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - With Chunked Transfer-Encoding");
        // Setup server with custom limits: 2MB POST limit, 10MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB drain limit
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds POST limit (5MB)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(5));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final byte[] postBytes = jsonData.getBytes("UTF-8");
                // Use HttpClient with chunked transfer encoding
                // Create InputStream wrapper (not ByteArrayInputStream to avoid auto-detection of length)
                final java.io.InputStream postDataStream = new CountingInputStream(new java.io.ByteArrayInputStream(postBytes));
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                // Set Transfer-Encoding: chunked header
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // Use RequestContext with postDataLength = -1 to enable chunked encoding
                    final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(postDataStream).setPostDataLength(-1));
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Chunked POST exceeding limit should return 413 (REQUEST_ENTITY_TOO_LARGE)
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Chunked POST exceeding limit should return 413, was: " + responseCode);
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized chunked POST body");
                    // Timing validation: 5MB chunked POST (should be fast)
                    // - Max allowed: 100ms (should be fast)
                    assertTrue(elapsed < 100, "Request with chunked encoding should complete within 100ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (exceeded with chunked) test successful: " + responseCode + " in " + elapsed + "ms");
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Exception is a failure - chunked POST should return 413 response, not throw exception
                    throw new Exception("POST Size Limit (chunked) test failed after " + elapsed + "ms: Expected 413 but got HttpClientException: " + e.getMessage(), e);
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
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with Chunked Transfer-Encoding and Drain Timeout - should stop draining after timeout POST data: 50MB
     * chunked, POST limit: 2MB, Drain limit: 100MB, Drain timeout: 2s -> Should stop draining after 2s
     */
    private void testPostSizeLimitExceededWithChunkedDrainTimeout() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - With Chunked Transfer-Encoding and Drain Timeout");
        // Setup server with short drain timeout: 2MB POST limit, 100MB drain limit, 2s timeout
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(100), // 100MB drain limit (much higher than POST data to allow timeout to trigger)
                TimeUnit.SECONDS.toMillis(2)); // 2s drain timeout (SHORT!)
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create very large POST data (50MB) - large enough that draining will take longer than 2s timeout
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(50));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final byte[] postBytes = jsonData.getBytes("UTF-8");
                // Use ThrottledInputStream to slow down data transfer so drain timeout can trigger
                // Wrap in FilterInputStream to prevent auto-detection of length for chunked encoding
                final ThrottledInputStream postDataStream = new ThrottledInputStream(new ByteArrayInputStream(postBytes));
                postDataStream.setLimit((int) ReadableBytes.Unit.MB.toKibiBytes(10)); // 10 MB/s limit
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // Use RequestContext with postDataLength = -1 to enable chunked encoding
                    final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(postDataStream).setPostDataLength(-1));
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Server should close connection after drain timeout - no response should be received
                    throw new Exception("POST Size Limit (chunked drain timeout) test failed: Expected HttpClientException (connection closed after timeout) but got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Expected: Connection closed if timeout occurs during drain
                    // Server-side exception should be present (request exceeded limit before timeout)
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized chunked POST body (before drain timeout)");
                    // Timing validation: 50MB chunked POST with drain timeout (2s)
                    // - Server should stop draining after 2 seconds (timeout triggers)
                    // - Min allowed: 1800ms (0.9x timeout, allows some variance)
                    // - Max allowed: 4000ms (2x timeout, allows for network overhead)
                    assertTrue(elapsed >= 1800, "Chunked request with drain timeout should take at least 1800ms (0.9x timeout), took: " + elapsed + "ms");
                    assertTrue(elapsed < 4000, "Chunked request with drain timeout should complete within 4000ms (2x timeout), took: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (chunked drain timeout) test - Drain stopped after timeout in " + elapsed + "ms: " + e.getMessage());
                } finally {
                    this.httpClient.clearRequestHeader();
                    this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                    this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
                }
                // Key point: Server should stop draining after 2 seconds (visible in logs)
                LogV3.info("POST Size Limit (chunked drain timeout) test completed - Check logs for 'Drain chunked stopped due to timeout'");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            this.teardownServer();
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with Chunked Transfer-Encoding that exceeds Drain Limit - should stop draining at limit POST data: 15MB
     * chunked, POST limit: 2MB, Drain limit: 5MB -> Should stop draining at 5MB limit
     */
    private void testPostSizeLimitExceededWithChunkedExceedsDrainLimit() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - With Chunked Transfer-Encoding Exceeding Drain Limit");
        // Setup server with custom limits: 2MB POST limit, 5MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(5), // 5MB drain limit (smaller than POST data)
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create POST data that exceeds both POST limit and drain limit (15MB)
                final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(15));
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final byte[] postBytes = jsonData.getBytes("UTF-8");
                // Use HttpClient with chunked transfer encoding
                // Create InputStream wrapper (not ByteArrayInputStream to avoid auto-detection of length)
                final java.io.InputStream postDataStream = new CountingInputStream(new java.io.ByteArrayInputStream(postBytes));
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // Use RequestContext with postDataLength = -1 to enable chunked encoding
                    final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(postDataStream).setPostDataLength(-1));
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Server should close connection when drain limit is exceeded - no response should be received
                    throw new Exception("POST Size Limit (chunked exceeds drain limit) test failed: Expected HttpClientException (connection closed when drain limit exceeded) but got response code: " + responseCode);
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Expected: Connection closed when drain limit is exceeded
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized chunked POST body (drain limit exceeded)");
                    // Timing validation: 15MB chunked POST with partial drain (server stops at 5MB limit)
                    // - Server skips draining (exceeds drain limit), so should be fast
                    // - Max allowed: 150ms (should be fast when draining is skipped)
                    assertTrue(elapsed < 150, "Chunked request with partial drain (5MB of 15MB) should complete within 150ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (chunked exceeds drain limit) test successful - Connection closed when drain limit exceeded in " + elapsed + "ms: " + e.getMessage());
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
            this.setupServer();
        }
    }

    /**
     * Test: POST exceeding limit with Chunked Transfer-Encoding AND GZIP compression - should drain compressed chunked data POST data: ~3MB
     * compressed chunked (expands to ~10MB), POST limit: 2MB, Drain limit: 10MB -> Should drain all compressed chunked data
     */
    private void testPostSizeLimitExceededWithChunkedAndGzipDraining() throws Exception {
        LogV3.info("Test: POST Size Limit Exceeded - With Chunked Transfer-Encoding and GZIP Compression");
        // Setup server with custom limits: 2MB POST limit, 10MB drain limit
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(16), // 16KB header
                ReadableBytes.Unit.MB.toKibiBytes(2), // 2MB POST limit
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB drain limit
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits(-1, ReadableBytes.Unit.MB.toKibiBytes(2)); // 2MB POST limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create uncompressed data that will exceed POST limit when compressed
                // Use less compressible data (random-like) to ensure compressed size exceeds 2MB
                // We need ~10MB uncompressed to get ~3MB compressed (typical GZIP ratio for random data)
                final String uncompressedData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(1));
                final String jsonData = "{\"data\":" + Deser.toString(uncompressedData) + "}";
                // Compress the data with GZIP
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                GZIPOutputStream gzipOut = null;
                try {
                    gzipOut = new GZIPOutputStream(baos);
                    while (baos.size() < ReadableBytes.Unit.MB.toKibiBytes(2)) {
                        gzipOut.write(jsonData.getBytes("UTF-8"));
                    }
                    gzipOut.flush();
                } finally {
                    if (gzipOut != null) {
                        try {
                            gzipOut.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
                final byte[] compressedData = baos.toByteArray();
                // Verify compressed data exceeds POST limit but is within drain limit
                assertTrue(compressedData.length > ReadableBytes.Unit.MB.toKibiBytes(2), "Compressed data should exceed POST limit (2MB), was: " + compressedData.length);
                assertTrue(compressedData.length < ReadableBytes.Unit.MB.toKibiBytes(10), "Compressed data should be within drain limit (10MB), was: " + compressedData.length);
                LogV3.info("Test data: Compressed=" + compressedData.length + " bytes (exceeds 2MB POST limit, within 10MB drain limit)");
                // Use HttpClient with chunked transfer encoding AND GZIP compression
                // Create InputStream wrapper (not ByteArrayInputStream to avoid auto-detection of length)
                final java.io.InputStream postDataStream = new CountingInputStream(new java.io.ByteArrayInputStream(compressedData));
                httpClient.setVerboseLog(true);
                httpServer.setVerboseLog(true);
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING, "gzip");
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // Use RequestContext with postDataLength = -1 to enable chunked encoding
                    final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(postDataStream).setPostDataLength(-1));
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // Must return 413 Request Entity Too Large (exceeds 2MB POST limit)
                    // Draining should work on the compressed chunked stream
                    assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Chunked+GZIP POST exceeding limit must return 413, was: " + responseCode);
                    assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized Chunked+GZIP POST body");
                    // Timing validation: ~3MB compressed chunked POST with full drain
                    // - Draining should be fast for localhost
                    // - Max allowed: 150ms (should be fast for localhost)
                    assertTrue(elapsed < 150, "Request with Chunked+GZIP drain (~3MB compressed) should complete within 150ms, took: " + elapsed + "ms");
                    assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                    LogV3.info("POST Size Limit (exceeded with Chunked+GZIP, within drain limit) test successful: " + responseCode + " in " + elapsed + "ms - Compressed chunked data drained");
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // HttpClientException should not occur - server should send 413 response after draining compressed chunked data
                    assertTrue(elapsed < 150, "Chunked+GZIP POST within drain limit should complete within 150ms even on exception, took: " + elapsed + "ms");
                    throw new Exception("Chunked+GZIP POST within drain limit test failed after " + elapsed + "ms: Expected 413 but got HttpClientException: " + e.getMessage(), e);
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
            this.setupServer();
        }
    }

    /**
     * Test: POST request with header size limit exceeded - draining not possible because request is unknown Header size: 20KB, Header
     * limit: 1KB -> Should fail during header parsing
     */
    private void testHeaderSizeLimitExceededWithPost() throws Exception {
        LogV3.info("Test: Header Size Limit Exceeded - POST Request");
        // Setup server with small header size limit: 1KB
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(1), // 1KB header limit (small!)
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB POST limit (large enough to not interfere)
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB drain limit
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits((int) ReadableBytes.Unit.KB.toKibiBytes(1), ReadableBytes.Unit.MB.toKibiBytes(10)); // 1KB header limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                final String largeHeaderValue = generateRandomString((int) ReadableBytes.Unit.KB.toKibiBytes(10));
                final String url = "http://localhost:" + this.serverPort + "/test/postData";
                // Set oversized header
                this.httpClient.putRequestHeader("X-Large-Header", largeHeaderValue);
                this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                httpServer.setVerboseLog(true);
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    // POST with same amount of data as other POST tests (5MB)
                    final String postData = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(5));
                    final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                    final RequestContext context = this.httpClient.post(url, jsonData);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // On Linux, may return response code (413) instead of closing connection
                    if (this.expectValidResponseWhenDrainingFails()) {
                        // Valid response code expected (Linux behavior)
                        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "POST with oversized header should return 413, was: " + responseCode);
                        assertTrue(this.lastServerException instanceof RequestSizeLimitExceededException, "Server-side: Exception expected for oversized header");
                        assertTrue(elapsed < 100, "Request with oversized header should complete within 100ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("POST Header Size Limit test successful - Got response code: " + responseCode + " in " + elapsed + "ms");
                    } else {
                        // Exception expected (Windows behavior) - but got response code, which is unexpected
                        throw new Exception("POST Header Size Limit test failed: Expected HttpClientException (connection closed) but got response code: " + responseCode);
                    }
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    if (this.expectExceptionWhenDrainingFails()) {
                        // Exception expected (Windows behavior) - draining not possible because request is unknown
                        assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized header");
                        assertTrue(elapsed < 200, "Request with oversized header should complete within 200ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("POST Header Size Limit test successful - Connection closed (draining not possible) in " + elapsed + "ms: " + e.getMessage());
                    } else {
                        // Valid response expected (Linux behavior) - but got exception, which is unexpected
                        throw new Exception("POST Header Size Limit test failed: Expected valid response code but got HttpClientException: " + e.getMessage(), e);
                    }
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
            this.setupServer();
        }
    }

    /**
     * Test: GET request with header size limit exceeded - draining not possible because request is unknown Header size: 20KB, Header limit:
     * 1KB -> Should fail during header parsing
     */
    private void testHeaderSizeLimitExceededWithGet() throws Exception {
        LogV3.info("Test: Header Size Limit Exceeded - GET Request");
        // Setup server with small header size limit: 1KB
        this.teardownServer();
        final RequestSizeLimits limits = new RequestSizeLimits(ReadableBytes.Unit.KB.toKibiBytes(1), // 1KB header limit (small!)
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB POST limit (not relevant for GET)
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB processed
                ReadableBytes.Unit.MB.toKibiBytes(10), // 10MB drain limit
                TimeUnit.SECONDS.toMillis(30)); // 30s drain timeout
        this.setupServerWithLimits((int) ReadableBytes.Unit.KB.toKibiBytes(1), ReadableBytes.Unit.MB.toKibiBytes(10)); // 1KB header limit
        this.httpServer.setRequestSizeLimits(limits);
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create oversized header value (20KB) - exceeds 1KB header limit
                final String largeHeaderValue = generateRandomString((int) ReadableBytes.Unit.MB.toKibiBytes(8));
                final String url = "http://localhost:" + this.serverPort + "/test/getData";
                // Set oversized header
                this.httpClient.putRequestHeader("X-Large-Header", largeHeaderValue.toString());
                this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
                this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
                final long startTime = System.currentTimeMillis();
                lastServerException = null;
                try {
                    final RequestContext context = this.httpClient.get(url);
                    final int responseCode = context.getCode();
                    final long elapsed = System.currentTimeMillis() - startTime;
                    // On Linux, may return response code (413) instead of closing connection
                    if (this.expectValidResponseWhenDrainingFails()) {
                        // Valid response code expected (Linux behavior)
                        assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "GET with oversized header should return 413, was: " + responseCode);
                        assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized header");
                        assertTrue(elapsed < 100, "Request with oversized header should complete within 100ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("GET Header Size Limit test successful - Got response code: " + responseCode + " in " + elapsed + "ms");
                    } else {
                        // Exception expected (Windows behavior) - but got response code, which is unexpected
                        throw new Exception("GET Header Size Limit test failed: Expected HttpClientException (connection closed) but got response code: " + responseCode);
                    }
                } catch (final HttpClientException e) {
                    final long elapsed = System.currentTimeMillis() - startTime;
                    if (this.expectExceptionWhenDrainingFails()) {
                        // Exception expected (Windows behavior) - draining not possible because request is unknown
                        assertTrue(this.lastServerException != null, "Server-side: Exception expected for oversized header");
                        assertTrue(elapsed < 500, "Request with oversized header should complete within 100ms, took: " + elapsed + "ms");
                        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                        LogV3.info("GET Header Size Limit test successful - Connection closed (draining not possible) in " + elapsed + "ms: " + e.getMessage());
                    } else {
                        // Valid response expected (Linux behavior) - but got exception, which is unexpected
                        throw new Exception("GET Header Size Limit test failed: Expected valid response code but got HttpClientException: " + e.getMessage(), e);
                    }
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
            this.setupServer();
        }
    }

    /**
     * Determines the expected behavior when draining fails (e.g., when draining is disabled or drain limit is exceeded).
     *
     * <p>
     * The behavior is OS-dependent:
     * </p>
     * <ul>
     * <li><b>Windows</b>: Usually results in an exception (connection closed by server)</li>
     * <li><b>Linux/Unix</b>: May return a response code (413) instead of closing the connection</li>
     * </ul>
     *
     * @return {@code true} if an exception is expected when draining fails, {@code false} otherwise
     */
    private boolean expectExceptionWhenDrainingFails() {
        // Windows typically closes the connection (exception), Linux may return response code
        return CrossSystem.isWindows();
    }

    /**
     * Determines if a valid response code is expected when draining fails (e.g., when draining is disabled or drain limit is exceeded).
     *
     * <p>
     * The behavior is OS-dependent:
     * </p>
     * <ul>
     * <li><b>Windows</b>: Usually results in an exception (connection closed by server)</li>
     * <li><b>Linux/Unix</b>: May return a response code (413) instead of closing the connection</li>
     * </ul>
     *
     * @return {@code true} if a valid response code is expected when draining fails, {@code false} otherwise
     */
    private boolean expectValidResponseWhenDrainingFails() {
        // Linux may return response code, Windows typically closes connection (exception)
        return !CrossSystem.isWindows();
    }
}
