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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.serializer.Deser;
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
 * <b>What is tested:</b> The codebase offers three ways to perform HTTP requests: (1) vanilla
 * {@link java.net.HttpURLConnection}, (2) AppWork's {@link BasicHTTP}, and (3) AppWork's
 * {@link org.appwork.utils.net.httpclient.HttpClient}. This test ensures they all behave
 * consistently and have comparable performance when talking to the same local HTTP server.
 * </p>
 *
 * <p>
 * <b>Test scenarios:</b>
 * </p>
 * <ul>
 * <li><b>POST 1KB:</b> Each client sends 100 POST requests with a 1KB JSON body to
 * {@code /test/postData}. We verify identical response codes and that no implementation is
 * unreasonably slower than the others (see performance rules below).</li>
 * <li><b>GET:</b> Same idea with 100 GET requests to {@code /test/echo?message=comparison}.</li>
 * </ul>
 *
 * <p>
 * <b>Assertions:</b>
 * </p>
 * <ul>
 * <li>All three clients must return the same HTTP response code for every request.</li>
 * <li>Each single request must finish within {@value #MAX_REQUEST_TIME_MS} ms (localhost).</li>
 * <li>Measured duration per request must be non-negative (0 ms is allowed due to clock resolution).</li>
 * <li>Performance ratio (slowest avg / fastest avg) must be at most 2x, <em>unless</em> the slowest
 * implementation averages below 10 ms per request – in that case we do not fail, since sub-10 ms
 * is considered fast enough regardless of relative difference.</li>
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
            this.testHttpClientComparisonGet();
        } finally {
            this.teardownServer();
        }
    }

    /** Number of requests per implementation; 100 gives statistically meaningful min/avg/median/max. */
    private static final int REQUESTS_PER_TYPE = 100;

    /** Pause between requests (ms) so we don't overload the local server. */
    private static final int DELAY_BETWEEN_REQUESTS_MS = 5;

    /** Upper bound for a single request duration (ms); requests taking longer are treated as failures. */
    private static final long MAX_REQUEST_TIME_MS = 500;

    /**
     * If the slowest implementation's average time per request is below this (ms), we skip the
     * 2x performance-ratio check: anything under 10 ms/request is considered acceptable.
     */
    private static final long MIN_AVG_MS_FOR_RATIO_CHECK = 10;

    /**
     * Runs the POST comparison: all three clients send {@value #REQUESTS_PER_TYPE} POST requests
     * with a 1KB JSON body to the server. We measure response code and duration per request, then
     * assert consistency and performance via {@link #assertAndLogTimings}.
     */
    private void testHttpClientComparison() throws Exception {
        LogV3.info("Test: HTTP Client Comparison POST (HttpURLConnection, BasicHTTP, HttpClient) – " + REQUESTS_PER_TYPE + " requests each");
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                /* Build 1KB JSON body and target URL for /test/postData. */
                final String postData = generateRandomString(1024);
                final String jsonData = "{\"data\":" + Deser.toString(postData) + "}";
                final byte[] jsonDataBytes = jsonData.getBytes("UTF-8");
                final String url = "http://localhost:" + this.serverPort + "/test/postData";

                final long[][] timingsPerType = new long[3][REQUESTS_PER_TYPE];
                final int[][] responseCodesPerType = new int[3][REQUESTS_PER_TYPE];
                final String[] implementationNames = { "HttpURLConnection (Vanilla Java)", "BasicHTTP", "HttpClient" };

                /* 1) Vanilla Java HttpURLConnection – warmup then measured requests. */
                LogV3.info("=== HttpURLConnection (Vanilla Java) – " + REQUESTS_PER_TYPE + " POST requests ===");
                testWithHTTPConnection(url, jsonDataBytes);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("HttpURLConnection Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithHTTPConnection(url, jsonDataBytes);
                    timingsPerType[0][i] = result[0];
                    responseCodesPerType[0][i] = (int) result[1];
                }

                Thread.sleep(500);

                /* 2) AppWork BasicHTTP – same URL and body. */
                LogV3.info("=== BasicHTTP – " + REQUESTS_PER_TYPE + " POST requests ===");
                testWithBasicHTTP(url, jsonDataBytes);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("BasicHTTP Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithBasicHTTP(url, jsonDataBytes);
                    timingsPerType[1][i] = result[0];
                    responseCodesPerType[1][i] = (int) result[1];
                }

                Thread.sleep(500);

                /* 3) AppWork HttpClient – same request. */
                LogV3.info("=== HttpClient – " + REQUESTS_PER_TYPE + " POST requests ===");
                testWithHttpClient(url, jsonData);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("HttpClient Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithHttpClient(url, jsonData);
                    timingsPerType[2][i] = result[0];
                    responseCodesPerType[2][i] = (int) result[1];
                }

                this.assertAndLogTimings(timingsPerType, responseCodesPerType, implementationNames, "POST 1KB");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            // Server cleanup handled by teardownServer() in runTest()
        }
    }

    /**
     * Runs the GET comparison: all three clients send {@value #REQUESTS_PER_TYPE} GET requests to
     * {@code /test/echo}. Same consistency and performance checks as for POST.
     */
    private void testHttpClientComparisonGet() throws Exception {
        LogV3.info("Test: HTTP Client Comparison GET (HttpURLConnection, BasicHTTP, HttpClient) – " + REQUESTS_PER_TYPE + " requests each");
        try {
            final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
            try {
                final String url = "http://localhost:" + this.serverPort + "/test/echo?message=comparison";

                final long[][] timingsPerType = new long[3][REQUESTS_PER_TYPE];
                final int[][] responseCodesPerType = new int[3][REQUESTS_PER_TYPE];
                final String[] implementationNames = { "HttpURLConnection (Vanilla Java)", "BasicHTTP", "HttpClient" };

                /* 1) HttpURLConnection GET. */
                LogV3.info("=== HttpURLConnection (Vanilla Java) – " + REQUESTS_PER_TYPE + " GET requests ===");
                testWithHTTPConnectionGET(url);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("HttpURLConnection GET Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithHTTPConnectionGET(url);
                    timingsPerType[0][i] = result[0];
                    responseCodesPerType[0][i] = (int) result[1];
                }

                Thread.sleep(500);

                /* 2) BasicHTTP GET. */
                LogV3.info("=== BasicHTTP – " + REQUESTS_PER_TYPE + " GET requests ===");
                testWithBasicHTTPGET(url);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("BasicHTTP GET Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithBasicHTTPGET(url);
                    timingsPerType[1][i] = result[0];
                    responseCodesPerType[1][i] = (int) result[1];
                }

                Thread.sleep(500);

                /* 3) HttpClient GET. */
                LogV3.info("=== HttpClient – " + REQUESTS_PER_TYPE + " GET requests ===");
                testWithHttpClientGET(url);
                Thread.sleep(100);
                for (int i = 0; i < REQUESTS_PER_TYPE; i++) {
                    if (i > 0) {
                        Thread.sleep(DELAY_BETWEEN_REQUESTS_MS);
                    }
                    if ((i + 1) % 25 == 0 || i == 0) {
                        LogV3.info("HttpClient GET Request " + (i + 1) + "/" + REQUESTS_PER_TYPE);
                    }
                    final long[] result = testWithHttpClientGET(url);
                    timingsPerType[2][i] = result[0];
                    responseCodesPerType[2][i] = (int) result[1];
                }

                this.assertAndLogTimings(timingsPerType, responseCodesPerType, implementationNames, "GET");
            } finally {
                this.restoreHttpMethods(previousMethods);
            }
        } finally {
            // Server cleanup handled by teardownServer() in runTest()
        }
    }

    /**
     * Evaluates timing and response-code data from all three clients.
     * <ul>
     * <li>Computes min/avg/median/max per implementation and logs them.</li>
     * <li>Asserts that every request returned the same HTTP response code.</li>
     * <li>Asserts performance ratio (slowest/fastest avg) is at most 2x, unless slowest avg &lt; 10 ms.</li>
     * <li>Asserts each request finished within {@value #MAX_REQUEST_TIME_MS} ms and duration is non-negative.</li>
     * </ul>
     *
     * @param timingsPerType   [implementation index][request index] = elapsed ms
     * @param responseCodesPerType [implementation index][request index] = HTTP response code
     * @param implementationNames labels for logging
     * @param scenario          e.g. "POST 1KB" or "GET" for log messages
     */
    private void assertAndLogTimings(final long[][] timingsPerType, final int[][] responseCodesPerType, final String[] implementationNames, final String scenario) throws Exception {
        final int n = timingsPerType[0].length;
        final long[] avgTimings = new long[3];
        final long[] minTimings = new long[3];
        final long[] maxTimings = new long[3];
        final long[] medianTimings = new long[3];

        /* Per-implementation stats (min, avg, median, max) and log. */
        for (int type = 0; type < 3; type++) {
            final long[] timings = timingsPerType[type];
            long min = Long.MAX_VALUE;
            long max = Long.MIN_VALUE;
            long sum = 0;
            for (int req = 0; req < n; req++) {
                final long t = timings[req];
                min = Math.min(min, t);
                max = Math.max(max, t);
                sum += t;
            }
            final long[] sorted = timings.clone();
            Arrays.sort(sorted);
            final long median = sorted[n / 2];
            final long avg = sum / n;
            avgTimings[type] = avg;
            minTimings[type] = min;
            maxTimings[type] = max;
            medianTimings[type] = median;
            LogV3.info(implementationNames[type] + " (" + scenario + "): Min=" + min + "ms, Avg=" + avg + "ms, Median=" + median + "ms, Max=" + max + "ms (Response: " + responseCodesPerType[type][0] + ")");
        }

        /* All implementations must return the same HTTP status for every request. */
        final int expectedResponseCode = responseCodesPerType[0][0];
        for (int type = 0; type < 3; type++) {
            for (int req = 0; req < n; req++) {
                assertTrue(responseCodesPerType[type][req] == expectedResponseCode,
                        implementationNames[type] + " request " + (req + 1) + " must return " + expectedResponseCode + ", was: " + responseCodesPerType[type][req]);
            }
        }

        /* Performance: slowest avg must be at most 2x fastest avg, unless slowest is already under 10 ms. */
        final long fastestAvg = Math.min(Math.min(avgTimings[0], avgTimings[1]), avgTimings[2]);
        final long slowestAvg = Math.max(Math.max(avgTimings[0], avgTimings[1]), avgTimings[2]);
        final double performanceRatio = (double) slowestAvg / fastestAvg;
        LogV3.info("Performance ratio (slowest/fastest): " + String.format("%.2f", performanceRatio) + "x");
        if (slowestAvg >= MIN_AVG_MS_FOR_RATIO_CHECK) {
            assertTrue(performanceRatio <= 2.0,
                    "Performance difference between implementations should be <= 2x, but slowest (" + slowestAvg + "ms) is " + String.format("%.2f", performanceRatio) + "x slower than fastest (" + fastestAvg + "ms)");
        } else {
            LogV3.info("Slowest avg " + slowestAvg + "ms < " + MIN_AVG_MS_FOR_RATIO_CHECK + "ms – skipping ratio check (under 10 ms/request is always OK)");
        }

        /* Each request must finish within the timeout and have non-negative duration (0 ms allowed). */
        for (int type = 0; type < 3; type++) {
            for (int req = 0; req < n; req++) {
                assertTrue(timingsPerType[type][req] < MAX_REQUEST_TIME_MS,
                        implementationNames[type] + " request " + (req + 1) + " should complete within " + MAX_REQUEST_TIME_MS + "ms, took: " + timingsPerType[type][req] + "ms");
                assertTrue(timingsPerType[type][req] >= 0,
                        implementationNames[type] + " request " + (req + 1) + " duration should be non-negative, was: " + timingsPerType[type][req] + "ms");
            }
        }

        LogV3.info("HTTP Client Comparison (" + scenario + ") successful – All implementations behave consistently");
    }

    /**
     * Executes a single GET request using vanilla {@link HttpURLConnection} (no AppWork code).
     * Used to compare behaviour and timing of the standard JDK client with BasicHTTP and HttpClient.
     *
     * @param urlString target URL (e.g. /test/echo?message=...)
     * @return [0] = elapsed time in ms, [1] = HTTP response code
     */
    private long[] testWithHTTPConnectionGET(final String urlString) throws Exception {
        final long startTime = System.currentTimeMillis();
        HttpURLConnection connection = null;
        try {
            final URL url = new URL(urlString);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty(HTTPConstants.X_APPWORK, "1");
            connection.setRequestProperty("Connection", "close");
            connection.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            connection.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
            connection.setDoInput(true);

            final int responseCode = connection.getResponseCode();
            try {
                InputStream inputStream = connection.getInputStream();
                final byte[] buffer = new byte[8192];
                while (inputStream.read(buffer) != -1) {
                    // Drain response body
                }
                inputStream.close();
            } catch (final IOException e) {
                try {
                    InputStream errorStream = connection.getErrorStream();
                    if (errorStream != null) {
                        final byte[] buffer = new byte[8192];
                        while (errorStream.read(buffer) != -1) {
                            // Drain
                        }
                        errorStream.close();
                    }
                } catch (final IOException ignore) {
                }
            }

            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("HttpURLConnection GET: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final IOException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("HttpURLConnection GET failed after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    /**
     * Executes a single POST request using vanilla {@link HttpURLConnection} (no AppWork code).
     * Sends the given body as JSON and drains the response so the connection is fully consumed.
     *
     * @param urlString target URL (e.g. /test/postData)
     * @param postData  raw bytes of the POST body (JSON)
     * @return [0] = elapsed time in ms, [1] = HTTP response code
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
            connection.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            connection.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
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
     * Executes a single GET request using AppWork's {@link BasicHTTP}.
     * Used in the GET comparison test alongside HttpURLConnection and HttpClient.
     *
     * @param urlString target URL
     * @return [0] = elapsed time in ms, [1] = HTTP response code
     */
    private long[] testWithBasicHTTPGET(final String urlString) throws Exception {
        final long startTime = System.currentTimeMillis();
        BasicHTTP basicHTTP = null;
        HTTPConnection connection = null;
        try {
            final URL url = new URL(urlString);
            basicHTTP = new BasicHTTP();
            basicHTTP.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            basicHTTP.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
            basicHTTP.getRequestHeader().put(HTTPConstants.X_APPWORK, "1");
            connection = basicHTTP.openGetConnection(url);
            connection.finalizeConnect();
            final int responseCode = connection.getResponseCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("BasicHTTP GET: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final BasicHTTPException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("BasicHTTP GET failed after " + elapsed + "ms: " + e.getMessage(), e);
        } catch (final InterruptedException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("BasicHTTP GET interrupted after " + elapsed + "ms: " + e.getMessage(), e);
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
     * Executes a single POST request using AppWork's {@link BasicHTTP}.
     * Sends the given bytes as the request body with Content-Type application/json.
     *
     * @param urlString target URL (e.g. /test/postData)
     * @param postData  raw bytes of the POST body
     * @return [0] = elapsed time in ms, [1] = HTTP response code
     */
    private long[] testWithBasicHTTP(final String urlString, final byte[] postData) throws Exception {
        final long startTime = System.currentTimeMillis();
        BasicHTTP basicHTTP = null;
        HTTPConnection connection = null;
        try {
            final URL url = new URL(urlString);
            basicHTTP = new BasicHTTP();
            basicHTTP.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            basicHTTP.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
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
     * Executes a single GET request using AppWork's {@link org.appwork.utils.net.httpclient.HttpClient}.
     * Uses the shared instance from the test base; headers and timeouts are reset in finally.
     *
     * @param urlString target URL
     * @return [0] = elapsed time in ms, [1] = HTTP response code
     */
    private long[] testWithHttpClientGET(final String urlString) throws Exception {
        final long startTime = System.currentTimeMillis();
        try {
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(50));
            this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(100));
            final RequestContext context = this.httpClient.get(urlString);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            LogV3.info("HttpClient GET: Response code " + responseCode + " in " + elapsed + "ms");
            return new long[] { elapsed, responseCode };
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            throw new Exception("HttpClient GET failed after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            this.httpClient.clearRequestHeader();
            this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
        }
    }

    /**
     * Executes a single POST request using AppWork's {@link org.appwork.utils.net.httpclient.HttpClient}.
     * Sends the given string as the POST body (Content-Type application/json). Resets headers and
     * timeouts in finally so the shared client is left in a clean state.
     *
     * @param urlString target URL (e.g. /test/postData)
     * @param postData  POST body string (e.g. JSON)
     * @return [0] = elapsed time in ms, [1] = HTTP response code
     */
    private long[] testWithHttpClient(final String urlString, final String postData) throws Exception {
        final long startTime = System.currentTimeMillis();
        try {
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
            throw new Exception("HttpClient test failed after " + elapsed + "ms: " + e.getMessage(), e);
        } finally {
            this.httpClient.clearRequestHeader();
            this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
        }
    }
}
