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

import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.URL;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;

/**
 * Tests for HTTP server attack scenarios and resilience.
 *
 * <p>
 * This test class verifies that the server handles various attack scenarios gracefully:
 * </p>
 * <ul>
 * <li>Normal requests work correctly</li>
 * <li>Long GET parameters are handled</li>
 * <li>Large POST bodies are handled</li>
 * <li>Many parameters are handled</li>
 * <li>Extremely long URLs are handled</li>
 * <li>Invalid headers are handled gracefully</li>
 * <li>Malformed JSON is handled</li>
 * <li>Concurrent requests are handled correctly</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerAttackScenariosTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testNormalRequest();
            this.testLongGetParameters();
            this.testLargePostBody();
            this.testVeryLargePostBodyWithinDrainLimit();
            this.testVeryLargePostBodyExceedsDrainLimit();
            this.testManyParameters();
            this.testExtremelyLongUrl();
            this.testInvalidHeaders();
            this.testMalformedJson();
            this.testConcurrentRequests();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Normal request should work
     */
    private void testNormalRequest() throws Exception {
        LogV3.info("Test: Normal Request");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncode.encodeURIComponent("Hello");
        // x-appwork header is already set in setupServer()
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        
        final long elapsed = System.currentTimeMillis() - startTime;
        
        assertTrue(responseCode == 200, "Response Code should be 200, was: " + responseCode);
        assertTrue(lastServerException == null, "Server-side: No exception expected for normal request, but got: " + lastServerException);
        
        // Timing validation: First request after server start (includes JVM warmup)
        // - Measured: 1003-1043ms across 5 runs (JVM warmup overhead!)
        // - Expected for subsequent requests: 5-50ms
        // - Max allowed: 1500ms (1.5x measured max, accounts for JVM warmup on slow machines)
        assertTrue(elapsed < 1500, "Normal request should complete within 1500ms, took: " + elapsed + "ms");
        assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
        
        LogV3.info("Normal request successful: " + responseCode + " in " + elapsed + "ms");
    }

    /**
     * Test: Too long GET parameters (URL should exceed default 16KB header limit)
     */
    private void testLongGetParameters() throws Exception {
        LogV3.info("Test: Too Long GET Parameters");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        // Create a very long parameter (20KB raw, encoded will be even larger) - exceeds default 16KB header limit
        final StringBuilder longParam = new StringBuilder();
        for (int i = 0; i < 20000; i++) {
            longParam.append("A");
        }

        final String encodedParam = URLEncode.encodeURIComponent(longParam);
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + encodedParam;
        // x-appwork header is already set in setupServer()
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            
            // Should return 413 Request Entity Too Large (exceeds 16KB default header limit)
            assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Long GET parameters should return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
            assertTrue(lastServerException != null, "Server-side: Exception expected for oversized header");
            
            // Timing validation: Oversized header (20KB) rejected during header parsing
            // - Measured: 44-46ms across 5 runs
            // - Max allowed: 100ms (2x measured max)
            assertTrue(elapsed < 100, "Oversized header should be rejected within 100ms, took: " + elapsed + "ms");
            
            LogV3.info("Long GET Parameters test successful: " + responseCode + " in " + elapsed + "ms, server exception: " + lastServerException.getClass().getSimpleName());
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            // HttpClientException can occur if server closes connection before reading all data
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                // Same timing constraint as 413 response case: 100ms
                assertTrue(elapsed < 100, "Oversized header should be rejected within 100ms, took: " + elapsed + "ms");
                LogV3.info("Long GET Parameters test: HttpClientException with limit message (expected) in " + elapsed + "ms: " + e.getMessage());
            } else {
                throw new Exception("Long GET Parameters test failed with unexpected HttpClientException: " + e.getMessage(), e);
            }
        }
    }

    /**
     * Test: Large POST body (1MB) - should be within default 2MB limit
     */
    private void testLargePostBody() throws Exception {
        LogV3.info("Test: Large POST Body (1MB) - within default 2MB limit");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Create 1MB data (within default 2MB limit)
            final StringBuilder largeData = new StringBuilder();
            for (int i = 0; i < 1024 * 1024; i++) {
                largeData.append("X");
            }
            final String jsonData = "{\"params\":[\"" + largeData.toString() + "\"]}";

            final String url = "http://localhost:" + this.serverPort + "/test/postData";
            // x-appwork header is already set in setupServer()
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            try {
                final RequestContext context = this.httpClient.post(url, jsonData);
                final int responseCode = context.getCode();
                final long elapsed = System.currentTimeMillis() - startTime;
                
                // Should return 200 (within 2MB default limit)
                assertTrue(responseCode == 200, "Large POST (1MB) should return 200, was: " + responseCode);
                assertTrue(lastServerException == null, "Server-side: No exception expected for 1MB POST within limit, but got: " + lastServerException);
                
                // Timing validation: 1MB POST within limit
                // - Measured: 53-77ms across 5 runs
                // - Max allowed: 150ms (2x measured max)
                assertTrue(elapsed < 150, "1MB POST should complete within 150ms, took: " + elapsed + "ms");
                assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                
                LogV3.info("Large POST Body test successful: " + responseCode + " in " + elapsed + "ms");
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
     * Test: Very large POST body (3MB) - exceeds default 2MB POST limit but within 10MB drain limit. Server should drain the data and
     * return 413 cleanly.
     */
    private void testVeryLargePostBodyWithinDrainLimit() throws Exception {
        LogV3.info("Test: Very Large POST Body (3MB) - exceeds 2MB POST limit, within 10MB drain limit");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Create 3MB data (exceeds default 2MB POST limit, but within 10MB drain limit)
            final StringBuilder veryLargeData = new StringBuilder();
            for (int i = 0; i < 3 * 1024 * 1024; i++) {
                veryLargeData.append("Y");
            }
            final String jsonData = "{\"params\":[\"" + veryLargeData.toString() + "\"]}";

            final String url = "http://localhost:" + this.serverPort + "/test/postData";
            // x-appwork header is already set in setupServer()
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            this.httpClient.setReadTimeout(60000);
            try {
                final RequestContext context = this.httpClient.post(url, jsonData);
                final int responseCode = context.getCode();
                final long elapsed = System.currentTimeMillis() - startTime;
                
                // Must return 413 Request Entity Too Large (exceeds 2MB POST limit)
                assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Very large POST (3MB) must return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
                assertTrue(lastServerException != null, "Server-side: Exception expected for oversized POST body");
                
                // Timing validation: 3MB POST with full drain (within 10MB drain limit)
                // - Measured: 30-42ms across 5 runs (drain 3MB in memory is very fast!)
                // - Max allowed: 100ms (2.5x measured max)
                assertTrue(elapsed < 100, "Request with drain (3MB) should complete within 100ms, took: " + elapsed + "ms");
                assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                
                LogV3.info("Very Large POST Body (within drain limit) test successful: " + responseCode + " in " + elapsed + "ms, server exception: " + lastServerException.getClass().getSimpleName());
            } catch (final HttpClientException e) {
                // HttpClientException should not occur - server should send 413 response after draining
                final long elapsed = System.currentTimeMillis() - startTime;
                throw new Exception("Very Large POST Body (within drain limit) test failed after " + elapsed + "ms: Expected 413 response code but got HttpClientException: " + e.getMessage(), e);
            } finally {
                this.httpClient.clearRequestHeader();
                // Restore mandatory header after clearing
                this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                this.httpClient.setReadTimeout(30000);
            }
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: Very large POST body (15MB) - exceeds default 2MB POST limit AND 10MB drain limit. Server should NOT drain all data (only up
     * to drain limit) and connection should be closed.
     */
    private void testVeryLargePostBodyExceedsDrainLimit() throws Exception {
        LogV3.info("Test: Very Large POST Body (15MB) - exceeds 2MB POST limit AND 10MB drain limit");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Create 15MB data (exceeds both 2MB POST limit AND 10MB drain limit)
            final StringBuilder veryLargeData = new StringBuilder();
            for (int i = 0; i < 15 * 1024 * 1024; i++) {
                veryLargeData.append("Z");
            }
            final String jsonData = "{\"params\":[\"" + veryLargeData.toString() + "\"]}";

            final String url = "http://localhost:" + this.serverPort + "/test/postData";
            // x-appwork header is already set in setupServer()
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            this.httpClient.setReadTimeout(60000);
            try {
                final RequestContext context = this.httpClient.post(url, jsonData);
                final int responseCode = context.getCode();
                final long elapsed = System.currentTimeMillis() - startTime;
                
                // Should fail - we don't expect a clean 413 here because drain limit is exceeded
                throw new Exception("Very Large POST Body (exceeds drain limit) test failed after " + elapsed + "ms: Expected HttpClientException but got response code: " + responseCode);
            } catch (final HttpClientException e) {
                final long elapsed = System.currentTimeMillis() - startTime;
                // HttpClientException is EXPECTED - server cannot drain all data and closes connection
                assertTrue(lastServerException != null, "Server-side: Exception expected for oversized POST body");
                
                // Timing validation: 15MB POST with partial drain (server stops at 10MB limit)
                // - Measured: 63-92ms across 5 runs (drain 10MB + close + client detects abort)
                // - Max allowed: 200ms (2x measured max)
                assertTrue(elapsed < 200, "Request with partial drain (10MB of 15MB) should complete within 200ms, took: " + elapsed + "ms");
                assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                
                LogV3.info("Very Large POST Body (exceeds drain limit) test successful: HttpClientException (expected) in " + elapsed + "ms: " + e.getMessage() + ", server exception: " + lastServerException.getClass().getSimpleName());
            } finally {
                this.httpClient.clearRequestHeader();
                // Restore mandatory header after clearing
                this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                this.httpClient.setReadTimeout(30000);
            }
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: Many GET parameters - should be within default 16KB header limit
     */
    private void testManyParameters() throws Exception {
        LogV3.info("Test: Many GET Parameters");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        final StringBuilder urlBuilder = new StringBuilder("http://localhost:" + this.serverPort + "/test/multiParam?");
        for (int i = 0; i < 1000; i++) {
            if (i > 0) {
                urlBuilder.append("&");
            }
            urlBuilder.append("param1=").append(URLEncode.encodeURIComponent("value" + i));
            urlBuilder.append("&param2=").append(URLEncode.encodeURIComponent("value" + i));
        }

        final String url = urlBuilder.toString();
        // x-appwork header is already set in setupServer()
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            
            // Should return 200 (within 16KB default header limit) or 413 if limit exceeded
            assertTrue(responseCode == 200 || responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Many Parameters should return 200 or " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + ", was: " + responseCode);
            
            if (responseCode == 200) {
                assertTrue(lastServerException == null, "Server-side: No exception expected for successful request, but got: " + lastServerException);
            } else {
                assertTrue(lastServerException != null, "Server-side: Exception expected for oversized header");
            }
            
            // Timing validation: 1000 parameters (~15-20KB URL)
            // - Measured: 38-43ms across 5 runs
            // - Max allowed: 100ms (2.5x measured max)
            assertTrue(elapsed < 100, "Many parameters request should complete within 100ms, took: " + elapsed + "ms");
            
            LogV3.info("Many Parameters test successful: " + responseCode + " in " + elapsed + "ms");
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            // HttpClientException can occur if URL is too long or limit exceeded
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                // Same timing constraint as success/413 response case: 100ms
                assertTrue(elapsed < 100, "Many parameters request should complete within 100ms, took: " + elapsed + "ms");
                LogV3.info("Many Parameters test: HttpClientException with limit message (expected) in " + elapsed + "ms: " + e.getMessage());
            } else {
                throw new Exception("Many Parameters test failed with unexpected HttpClientException: " + e.getMessage(), e);
            }
        }
    }

    /**
     * Test: Invalid headers - server should handle gracefully
     *
     * NOTE: This test uses HttpURLConnection directly instead of HttpClient because HttpClient validates headers and automatically sets
     * Content-Length based on POST data. It cannot send invalid headers like "Content-Length: invalid" or empty header names.
     */
    private void testInvalidHeaders() throws Exception {
        LogV3.info("Test: Invalid Headers");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        final URL url = new URL("http://localhost:" + this.serverPort + "/test/echo?message=test");
        final HttpURLConnection conn = (HttpURLConnection) url.openConnection(Proxy.NO_PROXY);
        conn.setConnectTimeout(5000);
        conn.setReadTimeout(5000);
        // Set mandatory header
        conn.setRequestProperty(HTTPConstants.X_APPWORK, "1");
        // Set invalid headers
        conn.setRequestProperty(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, "invalid");
        conn.setRequestProperty("", "empty header name");
        try {
            final int responseCode = conn.getResponseCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            
            // Server should handle invalid headers gracefully - either 200 or 400 Bad Request
            assertTrue(responseCode == 200 || responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Invalid Headers should return 200 or " + ResponseCode.ERROR_BAD_REQUEST.getCode() + ", was: " + responseCode);
            
            // Timing validation: Simple request with invalid headers
            // - Measured: 8-10ms across 5 runs
            // - Max allowed: 50ms (5x measured max)
            assertTrue(elapsed < 50, "Invalid headers request should complete within 50ms, took: " + elapsed + "ms");
            
            LogV3.info("Invalid Headers test successful: " + responseCode + " in " + elapsed + "ms");
        } finally {
            conn.disconnect();
        }
    }

    /**
     * Test: Extremely long URL - should exceed default 16KB header limit
     */
    private void testExtremelyLongUrl() throws Exception {
        LogV3.info("Test: Extremely Long URL - exceeds default 16KB header limit");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        final StringBuilder longPath = new StringBuilder("http://localhost:" + this.serverPort + "/test/echo?message=");
        // Create a very long URL (20KB raw, encoded will be even larger) - exceeds default 16KB header limit
        for (int i = 0; i < 20000; i++) {
            longPath.append("A");
        }

        final String url = longPath.toString();
        // x-appwork header is already set in setupServer()
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            final long elapsed = System.currentTimeMillis() - startTime;
            
            // Should return 413 Request Entity Too Large (exceeds 16KB default header limit)
            assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Extremely long URL should return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
            assertTrue(lastServerException != null, "Server-side: Exception expected for oversized URL");
            
            // Timing validation: Oversized URL (20KB) rejected during header parsing
            // - Measured: 35-40ms across 5 runs
            // - Max allowed: 100ms (2.5x measured max)
            assertTrue(elapsed < 100, "Oversized URL should be rejected within 100ms, took: " + elapsed + "ms");
            
            LogV3.info("Extremely Long URL test successful: " + responseCode + " in " + elapsed + "ms, server exception: " + lastServerException.getClass().getSimpleName());
        } catch (final HttpClientException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            // HttpClientException can occur if server closes connection before reading all data
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                // Same timing constraint as 413 response case: 100ms
                assertTrue(elapsed < 100, "Oversized URL should be rejected within 100ms, took: " + elapsed + "ms");
                LogV3.info("Extremely Long URL test: HttpClientException with limit message (expected) in " + elapsed + "ms: " + e.getMessage());
            } else {
                throw new Exception("Extremely Long URL test failed with unexpected HttpClientException: " + e.getMessage(), e);
            }
        } catch (final IllegalArgumentException e) {
            final long elapsed = System.currentTimeMillis() - startTime;
            // IllegalArgumentException can occur if URL is too long or malformed (client-side validation)
            // This should be instant (client-side check, no network involved)
            assertTrue(elapsed < 50, "Client-side URL validation should complete within 50ms, took: " + elapsed + "ms");
            LogV3.info("Extremely Long URL test: IllegalArgumentException (client-side, expected) in " + elapsed + "ms: " + e.getMessage());
        }
    }

    /**
     * Test: Malformed JSON in POST
     */
    private void testMalformedJson() throws Exception {
        LogV3.info("Test: Malformed JSON");
        lastServerException = null;
        final long startTime = System.currentTimeMillis();
        
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/postData";
            // Send invalid JSON
            final String invalidJson = "{invalid json}";
            // x-appwork header is already set in setupServer()
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            try {
                final RequestContext context = this.httpClient.post(url, invalidJson);
                final int responseCode = context.getCode();
                final long elapsed = System.currentTimeMillis() - startTime;
                
                // Server should return an error (400 Bad Request)
                assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Server should return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " error, was: " + responseCode);
                assertTrue(lastServerException != null, "Server-side: Exception expected for malformed JSON");
                
                // Timing validation: Small POST with malformed JSON
                // - Measured: 6-8ms across 5 runs
                // - Max allowed: 50ms (6x measured max)
                assertTrue(elapsed < 50, "Malformed JSON request should complete within 50ms, took: " + elapsed + "ms");
                assertTrue(elapsed > 0, "Request duration should be positive, was: " + elapsed + "ms");
                
                LogV3.info("Malformed JSON test successful: " + responseCode + " in " + elapsed + "ms, server exception: " + lastServerException.getClass().getSimpleName());
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
     * Test: Multiple concurrent requests
     */
    private void testConcurrentRequests() throws Exception {
        LogV3.info("Test: Concurrent Requests");
        final long startTime = System.currentTimeMillis();
        final int numThreads = 10;
        final Thread[] threads = new Thread[numThreads];
        final boolean[] success = new boolean[numThreads];
        final long[] requestTimes = new long[numThreads];

        for (int i = 0; i < numThreads; i++) {
            final int threadId = i;
            threads[i] = new Thread() {
                @Override
                public void run() {
                    try {
                        final long threadStartTime = System.currentTimeMillis();
                        final String url = "http://localhost:" + HttpServerAttackScenariosTest.this.serverPort + "/test/echo?message=" + URLEncode.encodeURIComponent("Thread" + threadId);
                        final org.appwork.utils.net.httpclient.HttpClient client = new org.appwork.utils.net.httpclient.HttpClient();
                        client.setConnectTimeout(5000);
                        client.setReadTimeout(5000);
                        client.putRequestHeader(HTTPConstants.X_APPWORK, "1"); // Set mandatory header
                        final RequestContext context = client.get(url);
                        final int responseCode = context.getCode();
                        requestTimes[threadId] = System.currentTimeMillis() - threadStartTime;
                        success[threadId] = (responseCode == 200);
                    } catch (final Exception e) {
                        LogV3.log(e);
                        success[threadId] = false;
                        requestTimes[threadId] = System.currentTimeMillis() - System.currentTimeMillis();
                    }
                }
            };
            threads[i].start();
        }

        // Wait for all threads
        for (int i = 0; i < numThreads; i++) {
            threads[i].join(10000);
        }

        final long totalElapsed = System.currentTimeMillis() - startTime;

        // Check results
        int successCount = 0;
        long minTime = Long.MAX_VALUE;
        long maxTime = 0;
        long totalTime = 0;
        for (int i = 0; i < numThreads; i++) {
            if (success[i]) {
                successCount++;
                minTime = Math.min(minTime, requestTimes[i]);
                maxTime = Math.max(maxTime, requestTimes[i]);
                totalTime += requestTimes[i];
            }
        }
        
        final long avgTime = successCount > 0 ? (totalTime / successCount) : 0;
        
        LogV3.info("Concurrent Requests test: " + successCount + "/" + numThreads + " successful in " + totalElapsed + "ms total (min: " + minTime + "ms, max: " + maxTime + "ms, avg: " + avgTime + "ms)");
        
        // All requests should be successful
        assertTrue(successCount == numThreads, "All " + numThreads + " requests should be successful, but only " + successCount + " were");
        
        // Timing validation: 10 concurrent GET requests
        // - Measured total time: 11-15ms across 5 runs (concurrent execution!)
        // - If executed sequentially, would take 10 * 10ms = 100ms+
        // - Max allowed total: 100ms (7x measured max, ensures concurrent execution)
        assertTrue(totalElapsed < 100, "Concurrent requests should complete within 100ms, took: " + totalElapsed + "ms");
        
        // Individual request timing
        // - Measured: 6-14ms per request
        // - Max allowed: 100ms (7x measured max)
        assertTrue(maxTime < 100, "Individual requests should complete within 100ms, slowest was: " + maxTime + "ms");
    }
}
