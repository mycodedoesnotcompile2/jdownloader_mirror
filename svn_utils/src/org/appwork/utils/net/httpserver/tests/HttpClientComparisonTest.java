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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.HashMap;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.BasicHTTP.BasicHTTP;
import org.appwork.utils.net.BasicHTTP.BasicHTTPException;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.RequestMethod;

/**
 * Tests comparing different HTTP client implementations (HttpURLConnection, BasicHTTP, HttpClient).
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>All three HTTP client implementations behave consistently</li>
 * <li>All implementations have similar performance characteristics</li>
 * <li>Response codes are identical across implementations</li>
 * <li>Timing differences are within acceptable ranges</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpClientComparisonTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testHttpClientComparison();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Compare all three HTTP client implementations (HttpURLConnection, BasicHTTP, HttpClient)
     * 
     * Verifies that all implementations:
     * - Return the same response codes
     * - Have similar performance (within 2x of each other)
     * - Handle requests consistently
     */
    private void testHttpClientComparison() throws Exception {
        LogV3.info("Test: HTTP Client Comparison (HttpURLConnection, BasicHTTP, HttpClient)");
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                // Create test POST data (1KB)
                final StringBuilder postData = new StringBuilder();
                for (int i = 0; i < 1024; i++) {
                    postData.append("X");
                }
                final String jsonData = "{\"data\":\"" + postData.toString() + "\"}";
                final byte[] jsonDataBytes = jsonData.getBytes("UTF-8");
                final String url = "http://localhost:" + this.serverPort + "/test/postData";

                // Test results storage - 3 requests per implementation type
                final int REQUESTS_PER_TYPE = 3;
                final long[][] timingsPerType = new long[3][REQUESTS_PER_TYPE]; // [type][request]
                final int[][] responseCodesPerType = new int[3][REQUESTS_PER_TYPE]; // [type][request]
                final String[] implementationNames = { "HttpURLConnection (Vanilla Java)", "BasicHTTP", "HttpClient" };

                // Test 1: HttpURLConnection (Vanilla Java) - 3 requests
                LogV3.info("=== Testing with HttpURLConnection (Vanilla Java) - " + REQUESTS_PER_TYPE + " requests ===");
                // Warmup request (ignored due to caching)
                LogV3.info("HttpURLConnection Warmup Request (ignored)");
                testWithHTTPConnection(url, jsonDataBytes);
                Thread.sleep(100);
                // Actual measurement requests
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    LogV3.info("HttpURLConnection Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    if (i > 0) {
                        Thread.sleep(100); // Small delay between requests
                    }
                    final long[] result = testWithHTTPConnection(url, jsonDataBytes);
                    timingsPerType[0][i] = result[0];
                    responseCodesPerType[0][i] = (int) result[1];
                }

                Thread.sleep(500); // Delay before switching implementation type

                // Test 2: BasicHTTP - 3 requests
                LogV3.info("=== Testing with BasicHTTP - " + REQUESTS_PER_TYPE + " requests ===");
                // Warmup request (ignored due to caching)
                LogV3.info("BasicHTTP Warmup Request (ignored)");
                testWithBasicHTTP(url, jsonDataBytes);
                Thread.sleep(100);
                // Actual measurement requests
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    LogV3.info("BasicHTTP Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    if (i > 0) {
                        Thread.sleep(100); // Small delay between requests
                    }
                    final long[] result = testWithBasicHTTP(url, jsonDataBytes);
                    timingsPerType[1][i] = result[0];
                    responseCodesPerType[1][i] = (int) result[1];
                }

                Thread.sleep(500); // Delay before switching implementation type

                // Test 3: HttpClient - 3 requests
                LogV3.info("=== Testing with HttpClient - " + REQUESTS_PER_TYPE + " requests ===");
                // Warmup request (ignored due to caching)
                LogV3.info("HttpClient Warmup Request (ignored)");
                testWithHttpClient(url, jsonData);
                Thread.sleep(100);
                // Actual measurement requests
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    LogV3.info("HttpClient Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    if (i > 0) {
                        Thread.sleep(100); // Small delay between requests
                    }
                    final long[] result = testWithHttpClient(url, jsonData);
                    timingsPerType[2][i] = result[0];
                    responseCodesPerType[2][i] = (int) result[1];
                }

                // Compare and report timings
                LogV3.info("=== Timing Comparison ===");
                final long[] avgTimings = new long[3];
                final long[] minTimings = new long[3];
                final long[] maxTimings = new long[3];
                for (int type = 0; type < 3; type++) {
                    long min = Long.MAX_VALUE;
                    long max = Long.MIN_VALUE;
                    long sum = 0;
                    for (int req = 0; req < REQUESTS_PER_TYPE; req++) {
                        final long timing = timingsPerType[type][req];
                        min = Math.min(min, timing);
                        max = Math.max(max, timing);
                        sum += timing;
                    }
                    final long avg = sum / REQUESTS_PER_TYPE;
                    avgTimings[type] = avg;
                    minTimings[type] = min;
                    maxTimings[type] = max;
                    LogV3.info(implementationNames[type] + ": Min=" + min + "ms, Avg=" + avg + "ms, Max=" + max + "ms (Response: " + responseCodesPerType[type][0] + ")");
                    // Log individual request timings
                    for (int req = 0; req < REQUESTS_PER_TYPE; req++) {
                        LogV3.info("  Request " + (req + 1) + ": " + timingsPerType[type][req] + "ms");
                    }
                }

                // Validate all responses are identical across implementations
                final int expectedResponseCode = responseCodesPerType[0][0];
                for (int type = 0; type < 3; type++) {
                    for (int req = 0; req < REQUESTS_PER_TYPE; req++) {
                        assertTrue(responseCodesPerType[type][req] == expectedResponseCode,
                                implementationNames[type] + " request " + (req + 1) + " must return " + expectedResponseCode + ", was: " + responseCodesPerType[type][req]);
                    }
                }

                // Performance validation: All implementations should be within 2x of each other
                final long fastestAvg = Math.min(Math.min(avgTimings[0], avgTimings[1]), avgTimings[2]);
                final long slowestAvg = Math.max(Math.max(avgTimings[0], avgTimings[1]), avgTimings[2]);
                final double performanceRatio = (double) slowestAvg / fastestAvg;
                LogV3.info("Performance ratio (slowest/fastest): " + String.format("%.2f", performanceRatio) + "x");
                assertTrue(performanceRatio <= 2.0,
                        "Performance difference between implementations should be <= 2x, but slowest (" + slowestAvg + "ms) is " + String.format("%.2f", performanceRatio) + "x slower than fastest (" + fastestAvg + "ms)");

                // Timing validation: all requests should complete within reasonable time (1KB POST should be very fast)
                for (int type = 0; type < 3; type++) {
                    for (int req = 0; req < REQUESTS_PER_TYPE; req++) {
                        assertTrue(timingsPerType[type][req] < 200,
                                implementationNames[type] + " request " + (req + 1) + " should complete within 200ms (1KB POST), took: " + timingsPerType[type][req] + "ms");
                        assertTrue(timingsPerType[type][req] > 0,
                                implementationNames[type] + " request " + (req + 1) + " duration should be positive, was: " + timingsPerType[type][req] + "ms");
                    }
                }

                LogV3.info("HTTP Client Comparison test successful - All implementations behave consistently");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            // Server cleanup handled by teardownServer() in runTest()
        }
    }

    /**
     * Test helper: Execute POST request using pure Java HttpURLConnection (no AppWork code, no 3rd party libraries)
     *
     * @return array with [0] = elapsed time in ms, [1] = response code
     */
    private long[] testWithHTTPConnection(final String urlString, final byte[] postData) throws Exception {
        final long startTime = System.currentTimeMillis();
        HttpURLConnection connection = null;
        try {
            final URL url = new URL(urlString);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty(HTTPConstants.X_APPWORK, "1");
            connection.setRequestProperty("Content-Length", String.valueOf(postData.length));
            connection.setRequestProperty("Connection", "close"); // Disable connection reuse
            connection.setConnectTimeout(50000);
            connection.setReadTimeout(100000);
            connection.setDoOutput(true);
            connection.setDoInput(true);

            // Write POST data
            try (OutputStream outputStream = connection.getOutputStream()) {
                outputStream.write(postData);
                outputStream.flush();
            }

            // Read response
            final int responseCode = connection.getResponseCode();

            // Read response body to ensure connection is fully processed and cleaned up
            try {
                InputStream inputStream = connection.getInputStream();
                final byte[] buffer = new byte[8192];
                while (inputStream.read(buffer) != -1) {
                    // Drain response body to ensure complete connection cleanup
                }
                inputStream.close();
            } catch (final IOException e) {
                // For error responses (e.g., 413), try error stream instead
                try {
                    InputStream errorStream = connection.getErrorStream();
                    if (errorStream != null) {
                        final byte[] buffer = new byte[8192];
                        while (errorStream.read(buffer) != -1) {
                            // Drain error response body
                        }
                        errorStream.close();
                    }
                } catch (final IOException ignore) {
                    // Ignore - connection cleanup is best effort
                }
            }

            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("HttpURLConnection (Vanilla Java): Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final IOException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("HttpURLConnection (Vanilla Java) test failed after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    /**
     * Test helper: Execute POST request using BasicHTTP
     *
     * @return array with [0] = elapsed time in ms, [1] = response code
     */
    private long[] testWithBasicHTTP(final String urlString, final byte[] postData) throws Exception {
        final long startTime = System.currentTimeMillis();
        BasicHTTP basicHTTP = null;
        HTTPConnection connection = null;
        try {
            final URL url = new URL(urlString);
            basicHTTP = new BasicHTTP();
            basicHTTP.setConnectTimeout(50000);
            basicHTTP.setReadTimeout(100000);
            final HashMap<String, String> headers = new HashMap<String, String>();
            headers.put(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            headers.put(HTTPConstants.X_APPWORK, "1");
            final InputStream postDataStream = new ByteArrayInputStream(postData);
            connection = basicHTTP.openPostConnection(url, null, postDataStream, headers, postData.length);
            connection.finalizeConnect();
            final int responseCode = connection.getResponseCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("BasicHTTP: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final BasicHTTPException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("BasicHTTP test failed after " + elapsed + "ms: " + e.getMessage(), e);
        } catch (final InterruptedException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("BasicHTTP test interrupted after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            if (connection != null) {
                try {
                    connection.disconnect();
                } catch (final Throwable ignore) {
                }
            }
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
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            this.httpClient.setConnectTimeout(50000);
            this.httpClient.setReadTimeout(100000);
            final RequestContext context = this.httpClient.post(urlString, postData);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("HttpClient: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("HttpClient test failed after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            this.httpClient.clearRequestHeader();
            this.httpClient.setConnectTimeout(5000);
            this.httpClient.setReadTimeout(30000);
        }
    }
}
