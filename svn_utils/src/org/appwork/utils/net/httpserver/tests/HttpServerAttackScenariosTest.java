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
            this.testVeryLargePostBody();
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
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncode.encodeURIComponent("Hello");
        // x-appwork header is already set in setupServer()
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        assertTrue(responseCode == 200, "Response Code should be 200, was: " + responseCode);
        LogV3.info("Normal request successful: " + responseCode);
    }

    /**
     * Test: Too long GET parameters (URL should exceed default 16KB header limit)
     */
    private void testLongGetParameters() throws Exception {
        LogV3.info("Test: Too Long GET Parameters");
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
            // Should return 413 Request Entity Too Large (exceeds 16KB default header limit)
            assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Long GET parameters should return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
            LogV3.info("Long GET Parameters test successful: " + responseCode);
        } catch (final HttpClientException e) {
            // HttpClientException can occur if server closes connection before reading all data
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                LogV3.info("Long GET Parameters test: HttpClientException with limit message (expected): " + e.getMessage());
            } else {
                throw new Exception("Long GET Parameters test failed with unexpected HttpClientException: " + e.getMessage(), e);
            }
        }
    }

    /**
     * Test: Large POST body (1MB) - should be within default 10MB limit
     */
    private void testLargePostBody() throws Exception {
        LogV3.info("Test: Large POST Body (1MB) - within default 10MB limit");
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Create 1MB data (within default 10MB limit)
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
                // Should return 200 (within 10MB default limit)
                assertTrue(responseCode == 200, "Large POST (1MB) should return 200, was: " + responseCode);
                LogV3.info("Large POST Body test successful: " + responseCode);
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
     * Test: Very large POST body (11MB) - should exceed default 10MB limit
     */
    private void testVeryLargePostBody() throws Exception {
        LogV3.info("Test: Very Large POST Body (11MB) - exceeds default 10MB limit");
        // Allow POST method for this test
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            // Create 11MB data (exceeds default 10MB limit)
            final StringBuilder veryLargeData = new StringBuilder();
            for (int i = 0; i < 11 * 1024 * 1024; i++) {
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
                // Must return 413 Request Entity Too Large (exceeds 10MB default limit)
                assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Very large POST (11MB) must return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
                LogV3.info("Very Large POST Body test successful: " + responseCode);
            } catch (final HttpClientException e) {
                // HttpClientException should not occur - server should send 413 response before closing
                throw new Exception("Very Large POST Body test failed: Expected 413 response code but got HttpClientException: " + e.getMessage(), e);
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
            // Should return 200 (within 16KB default header limit) or 413 if limit exceeded
            assertTrue(responseCode == 200 || responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Many Parameters should return 200 or " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + ", was: " + responseCode);
            LogV3.info("Many Parameters test successful: " + responseCode);
        } catch (final HttpClientException e) {
            // HttpClientException can occur if URL is too long or limit exceeded
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                LogV3.info("Many Parameters test: HttpClientException with limit message (expected): " + e.getMessage());
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
            // Server should handle invalid headers gracefully - either 200 or 400 Bad Request
            assertTrue(responseCode == 200 || responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Invalid Headers should return 200 or " + ResponseCode.ERROR_BAD_REQUEST.getCode() + ", was: " + responseCode);
            LogV3.info("Invalid Headers test successful: " + responseCode);
        } finally {
            conn.disconnect();
        }
    }

    /**
     * Test: Extremely long URL - should exceed default 16KB header limit
     */
    private void testExtremelyLongUrl() throws Exception {
        LogV3.info("Test: Extremely Long URL - exceeds default 16KB header limit");
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
            // Should return 413 Request Entity Too Large (exceeds 16KB default header limit)
            assertTrue(responseCode == ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode(), "Extremely long URL should return " + ResponseCode.REQUEST_ENTITY_TOO_LARGE.getCode() + " error, was: " + responseCode);
            LogV3.info("Extremely Long URL test successful: " + responseCode);
        } catch (final HttpClientException e) {
            // HttpClientException can occur if server closes connection before reading all data
            if (e.getMessage() != null && (e.getMessage().contains("limit") || e.getMessage().contains("exceeded"))) {
                LogV3.info("Extremely Long URL test: HttpClientException with limit message (expected): " + e.getMessage());
            } else {
                throw new Exception("Extremely Long URL test failed with unexpected HttpClientException: " + e.getMessage(), e);
            }
        } catch (final IllegalArgumentException e) {
            // IllegalArgumentException can occur if URL is too long or malformed
            LogV3.info("Extremely Long URL test: IllegalArgumentException (possibly expected): " + e.getMessage());
        }
    }

    /**
     * Test: Malformed JSON in POST
     */
    private void testMalformedJson() throws Exception {
        LogV3.info("Test: Malformed JSON");
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
                // Server should return an error (400 Bad Request)
                assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Server should return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " error, was: " + responseCode);
                LogV3.info("Malformed JSON test successful: " + responseCode);
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
        final int numThreads = 10;
        final Thread[] threads = new Thread[numThreads];
        final boolean[] success = new boolean[numThreads];

        for (int i = 0; i < numThreads; i++) {
            final int threadId = i;
            threads[i] = new Thread() {
                @Override
                public void run() {
                    try {
                        final String url = "http://localhost:" + HttpServerAttackScenariosTest.this.serverPort + "/test/echo?message=" + URLEncode.encodeURIComponent("Thread" + threadId);
                        final org.appwork.utils.net.httpclient.HttpClient client = new org.appwork.utils.net.httpclient.HttpClient();
                        client.setConnectTimeout(5000);
                        client.setReadTimeout(5000);
                        client.putRequestHeader(HTTPConstants.X_APPWORK, "1"); // Set mandatory header
                        final RequestContext context = client.get(url);
                        final int responseCode = context.getCode();
                        success[threadId] = (responseCode == 200);
                    } catch (final Exception e) {
                        LogV3.log(e);
                        success[threadId] = false;
                    }
                }
            };
            threads[i].start();
        }

        // Wait for all threads
        for (int i = 0; i < numThreads; i++) {
            threads[i].join(10000);
        }

        // Check results
        int successCount = 0;
        for (int i = 0; i < numThreads; i++) {
            if (success[i]) {
                successCount++;
            }
        }
        LogV3.info("Concurrent Requests test: " + successCount + "/" + numThreads + " successful");
        // At least some requests should be successful
        assertTrue(successCount > 0, "At least one request should be successful");
    }
}
