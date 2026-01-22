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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.responsewrapper.DataObject;
import org.appwork.serializer.Deser;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.utils.Application;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;

/**
 * Comprehensive test for HTTP server that verifies exact request and response headers.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>All request headers sent to the server are exactly as expected (none missing, none extra) - case-sensitive</li>
 * <li>All response headers from the server are exactly as expected (none missing, none extra) - case-sensitive</li>
 * <li>The server correctly processes and returns all request headers</li>
 * <li>Any changes in header names (case changes) or missing/extra headers are detected</li>
 * </ul>
 *
 * <p>
 * The test uses the getAllRequestHeaders endpoint which returns all request headers as JSON, allowing the test to verify that exactly the
 * expected headers are present. Each test verifies both request and response headers to ensure complete coverage.
 * </p>
 *
 * @author AppWork
 */
public class HttpServerHeaderVerificationTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testHeadersGET();
            this.testHeadersPOST();
            this.testHeadersWithCustomHeaders();
            this.testHeadersCaseHandling();
            this.testContentLengthHeader();
            this.testSelfVerification();
            LogV3.info("All header verification tests passed!");
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Verifies exact request and response headers for GET requests.
     *
     * <p>
     * <b>Purpose:</b> This test ensures that GET requests send exactly the expected request headers and receive exactly the expected
     * response headers. Any deviation (missing headers, extra headers, or case changes) will be detected.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>Request headers: All standard headers (Accept-Language, User-Agent, Accept-Charset, Connection, Host, Content-Length,
     * x-appwork)</li>
     * <li>Response headers: All security headers (X-Content-Type-Options, Content-Security-Policy, Referrer-Policy), Content-Type,
     * Connection, Cache-Control, Server, Content-Length</li>
     * <li>Case-sensitivity: Header names must match exactly (e.g., "Content-Type" not "content-type")</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test detects any changes in the HTTP server's behavior, such as:
     * </p>
     * <ul>
     * <li>New headers being added automatically</li>
     * <li>Headers being removed</li>
     * <li>Case changes in header names (which could break clients expecting specific case)</li>
     * <li>Changes in header values</li>
     * </ul>
     */
    private void testHeadersGET() throws Exception {
        LogV3.info("Test: Request and Response Headers for GET");
        final String url = "http://localhost:" + this.serverPort + "/test/getAllRequestHeaders";

        final Map<String, String> expectedRequestHeaders = this.getStandardHeaders();
        expectedRequestHeaders.put("x-appwork", "1");
        expectedRequestHeaders.put("Host", "localhost:" + this.serverPort);
        expectedRequestHeaders.put("Content-Length", "0");

        final RequestContext context = this.httpClient.get(url);
        assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");

        final Map<String, String> actualRequestHeaders = this.parseHeadersJson(context.getResponseString());
        this.verifyHeadersMatchExactly(expectedRequestHeaders, actualRequestHeaders, "GET request headers", context);

        final Map<String, String> actualResponseHeaders = this.getResponseHeaders(context);
        final Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(actualResponseHeaders);
        this.verifyHeadersMatchExactly(expectedResponseHeaders, actualResponseHeaders, "GET response headers", context);
    }

    /**
     * Test: Verifies exact request and response headers for POST requests.
     *
     * <p>
     * <b>Purpose:</b> This test ensures that POST requests send exactly the expected request headers (including correct Content-Length) and
     * receive exactly the expected response headers. Any deviation will be detected.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>Request headers: All standard headers plus correct Content-Length for POST body</li>
     * <li>Response headers: All security headers and standard response headers</li>
     * <li>Content-Length: Must match the actual POST body size exactly</li>
     * <li>Case-sensitivity: All header names must match exactly</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test detects:
     * </p>
     * <ul>
     * <li>Incorrect Content-Length calculation for POST requests</li>
     * <li>Missing or extra headers in POST requests</li>
     * <li>Changes in response headers for POST requests</li>
     * <li>Case changes in header names</li>
     * </ul>
     */
    private void testHeadersPOST() throws Exception {
        LogV3.info("Test: Request and Response Headers for POST");
        final String url = "http://localhost:" + this.serverPort + "/test/getAllRequestHeaders";

        final byte[] postData = "test=data".getBytes("UTF-8");
        final int postDataLength = postData.length;

        final Map<String, String> expectedRequestHeaders = this.getStandardHeaders();
        expectedRequestHeaders.put("x-appwork", "1");
        expectedRequestHeaders.put("Host", "localhost:" + this.serverPort);
        expectedRequestHeaders.put("Content-Length", String.valueOf(postDataLength));

        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
        try {
            final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(postData)).setPostDataLength(postDataLength));
            assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");

            final Map<String, String> actualRequestHeaders = this.parseHeadersJson(context.getResponseString());
            this.verifyHeadersMatchExactly(expectedRequestHeaders, actualRequestHeaders, "POST request headers", context);

            final Map<String, String> actualResponseHeaders = this.getResponseHeaders(context);
            final Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(actualResponseHeaders);
            this.verifyHeadersMatchExactly(expectedResponseHeaders, actualResponseHeaders, "POST response headers", context);
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: Verifies request and response headers when custom headers are added.
     *
     * <p>
     * <b>Purpose:</b> This test ensures that custom headers are correctly sent in requests and that response headers remain unchanged when
     * custom request headers are present.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>Request headers: Standard headers plus custom headers (X-Custom-Header, X-Another-Header)</li>
     * <li>Response headers: All expected response headers (should not be affected by custom request headers)</li>
     * <li>Custom header preservation: Custom headers must be sent exactly as specified (case-sensitive)</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test detects:
     * </p>
     * <ul>
     * <li>Custom headers being lost or modified</li>
     * <li>Case changes in custom header names</li>
     * <li>Response headers being affected by custom request headers (should not happen)</li>
     * </ul>
     */
    private void testHeadersWithCustomHeaders() throws Exception {
        LogV3.info("Test: Headers with Custom Request Headers");
        final String url = "http://localhost:" + this.serverPort + "/test/getAllRequestHeaders";

        final org.appwork.utils.net.httpclient.HttpClient testClient = new org.appwork.utils.net.httpclient.HttpClient();
        testClient.setConnectTimeout(5000);
        testClient.setReadTimeout(30000);
        testClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        testClient.putRequestHeader("X-Custom-Header", "custom-value");
        testClient.putRequestHeader("X-Another-Header", "another-value");

        final Map<String, String> expectedRequestHeaders = this.getStandardHeaders();
        expectedRequestHeaders.put("x-appwork", "1");
        expectedRequestHeaders.put("X-Custom-Header", "custom-value");
        expectedRequestHeaders.put("X-Another-Header", "another-value");
        expectedRequestHeaders.put("Host", "localhost:" + this.serverPort);
        expectedRequestHeaders.put("Content-Length", "0");

        final RequestContext context = testClient.get(url);
        assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");

        final Map<String, String> actualRequestHeaders = this.parseHeadersJson(context.getResponseString());
        this.verifyHeadersMatchExactly(expectedRequestHeaders, actualRequestHeaders, "Custom headers request", context);

        final Map<String, String> actualResponseHeaders = this.getResponseHeaders(context);
        final Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(actualResponseHeaders);
        this.verifyHeadersMatchExactly(expectedResponseHeaders, actualResponseHeaders, "Custom headers response", context);
    }

    /**
     * Test: Verifies that headers can be found case-insensitively (HTTP standard), but the test itself uses case-sensitive comparison.
     *
     * <p>
     * <b>Purpose:</b> This test verifies that the server correctly handles headers with different cases (HTTP headers are
     * case-insensitive), while ensuring that the test framework itself uses case-sensitive comparison to detect any case changes.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>Request headers: Headers sent with different case (X-APPWORK instead of x-appwork) are correctly processed</li>
     * <li>Response headers: All expected response headers are present</li>
     * <li>Case-insensitive lookup: Headers can be found regardless of case (HTTP standard behavior)</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test ensures:
     * </p>
     * <ul>
     * <li>The server correctly handles case variations in headers (HTTP compliance)</li>
     * <li>The test framework can still detect case changes when comparing expected vs actual headers</li>
     * </ul>
     */
    private void testHeadersCaseHandling() throws Exception {
        LogV3.info("Test: Headers Case Handling");
        final String url = "http://localhost:" + this.serverPort + "/test/getAllRequestHeaders";

        final org.appwork.utils.net.httpclient.HttpClient testClient = new org.appwork.utils.net.httpclient.HttpClient();
        testClient.setConnectTimeout(5000);
        testClient.setReadTimeout(30000);
        testClient.putRequestHeader("X-APPWORK", "1");
        testClient.putRequestHeader("X-Custom-Header", "test-value");

        final RequestContext context = testClient.get(url);
        assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");

        final Map<String, String> actualRequestHeaders = this.parseHeadersJson(context.getResponseString());
        final String xAppworkValue = this.findHeaderIgnoreCase(actualRequestHeaders, HTTPConstants.X_APPWORK);
        assertTrue("1".equals(xAppworkValue), "X-Appwork header should be present with value '1', found: " + xAppworkValue);

        final String customHeaderValue = this.findHeaderIgnoreCase(actualRequestHeaders, "X-Custom-Header");
        assertTrue("test-value".equals(customHeaderValue), "X-Custom-Header should be present with value 'test-value', found: " + customHeaderValue);

        final Map<String, String> actualResponseHeaders = this.getResponseHeaders(context);
        final Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(actualResponseHeaders);
        this.verifyHeadersMatchExactly(expectedResponseHeaders, actualResponseHeaders, "Case insensitive response headers", context);
    }

    /**
     * Test: Verifies Content-Length header is correct for different request types and body sizes.
     *
     * <p>
     * <b>Purpose:</b> This test ensures that the Content-Length header is correctly set for GET requests (0) and POST requests with various
     * body sizes (small, large, empty). It also verifies that response headers remain correct for all scenarios.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>GET request: Content-Length must be 0</li>
     * <li>POST with small body: Content-Length must match body size exactly</li>
     * <li>POST with large body (1KB): Content-Length must match body size exactly</li>
     * <li>POST with empty body: Content-Length must be 0</li>
     * <li>Response headers: All expected response headers for each request type</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test detects:
     * </p>
     * <ul>
     * <li>Incorrect Content-Length calculation</li>
     * <li>Missing Content-Length header</li>
     * <li>Content-Length mismatches that could cause HTTP protocol errors</li>
     * </ul>
     */
    private void testContentLengthHeader() throws Exception {
        LogV3.info("Test: Content-Length Header Verification");
        final String url = "http://localhost:" + this.serverPort + "/test/getAllRequestHeaders";

        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
        try {
            RequestContext context = this.httpClient.get(url);
            assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");
            Map<String, String> requestHeaders = this.parseHeadersJson(context.getResponseString());
            assertTrue("0".equals(requestHeaders.get("Content-Length")), "GET request should have Content-Length=0");
            Map<String, String> responseHeaders = this.getResponseHeaders(context);
            Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(responseHeaders);
            this.verifyHeadersMatchExactly(expectedResponseHeaders, responseHeaders, "GET Content-Length response headers", context);

            final byte[] smallBody = "test=data".getBytes("UTF-8");
            final int smallBodyLength = smallBody.length;
            context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(smallBody)).setPostDataLength(smallBodyLength));
            assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");
            requestHeaders = this.parseHeadersJson(context.getResponseString());
            assertTrue(String.valueOf(smallBodyLength).equals(requestHeaders.get("Content-Length")), "POST request should have Content-Length=" + smallBodyLength);
            responseHeaders = this.getResponseHeaders(context);
            expectedResponseHeaders = this.getExpectedResponseHeaders(responseHeaders);
            this.verifyHeadersMatchExactly(expectedResponseHeaders, responseHeaders, "POST small body response headers", context);

            final byte[] largeBody = new byte[1024];
            for (int i = 0; i < largeBody.length; i++) {
                largeBody[i] = (byte) (i % 256);
            }
            context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(largeBody)).setPostDataLength(largeBody.length));
            assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");
            requestHeaders = this.parseHeadersJson(context.getResponseString());
            assertTrue(String.valueOf(largeBody.length).equals(requestHeaders.get("Content-Length")), "POST request should have Content-Length=" + largeBody.length);
            responseHeaders = this.getResponseHeaders(context);
            expectedResponseHeaders = this.getExpectedResponseHeaders(responseHeaders);
            this.verifyHeadersMatchExactly(expectedResponseHeaders, responseHeaders, "POST large body response headers", context);

            context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(new byte[0])).setPostDataLength(0));
            assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");
            requestHeaders = this.parseHeadersJson(context.getResponseString());
            assertTrue("0".equals(requestHeaders.get("Content-Length")), "POST request with empty body should have Content-Length=0");
            responseHeaders = this.getResponseHeaders(context);
            expectedResponseHeaders = this.getExpectedResponseHeaders(responseHeaders);
            this.verifyHeadersMatchExactly(expectedResponseHeaders, responseHeaders, "POST empty body response headers", context);
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: Self-verification - Verifies that the test framework itself correctly detects missing headers.
     *
     * <p>
     * <b>Purpose:</b> This test verifies that the test framework is working correctly by intentionally removing a header
     * (X-Content-Type-Options) and ensuring that the test detects this missing header. This is a meta-test that ensures the test itself is
     * functioning properly.
     * </p>
     *
     * <p>
     * <b>What it tests:</b>
     * </p>
     * <ul>
     * <li>Test framework correctness: The test must detect when a required header is missing</li>
     * <li>Response headers: After removing X-Content-Type-Options, the test should fail with a clear error message</li>
     * <li>Test reliability: Ensures that the test framework will catch real issues, not just pass silently</li>
     * </ul>
     *
     * <p>
     * <b>Why it's important:</b> This test ensures:
     * </p>
     * <ul>
     * <li>The test framework is working correctly and will detect real problems</li>
     * <li>Missing headers are properly reported with clear error messages</li>
     * <li>The test doesn't have false positives (passing when it should fail)</li>
     * </ul>
     */
    private void testSelfVerification() throws Exception {
        LogV3.info("Test: Self-Verification - Test detects missing headers");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        ResponseSecurityHeaders sec = new ResponseSecurityHeaders();
        sec.setXContentTypeOptions(null);
        httpServer.setResponseSecurityHeaders(sec);

        final RequestContext context = this.httpClient.get(url);
        assertTrue(context.getCode() == ResponseCode.SUCCESS_OK.getCode(), "Request should return 200");

        final Map<String, String> actualResponseHeaders = this.getResponseHeaders(context);
        final Map<String, String> expectedResponseHeaders = this.getExpectedResponseHeaders(actualResponseHeaders);

        boolean caughtException = false;
        try {
            this.verifyHeadersMatchExactly(expectedResponseHeaders, actualResponseHeaders, "Self-verification test", context);
        } catch (final AssertionError e) {
            caughtException = true;
            final String errorMsg = e.getMessage();
            assertTrue(errorMsg.contains("Missing headers") || errorMsg.contains("X-Content-Type-Options"), "Test should detect missing X-Content-Type-Options header. Error: " + errorMsg);
        }
        assertTrue(caughtException, "Test should have detected missing X-Content-Type-Options header");

        sec = new ResponseSecurityHeaders();
        httpServer.setResponseSecurityHeaders(sec);
    }

    private Map<String, String> getStandardHeaders() {
        final Map<String, String> headers = new HashMap<String, String>();
        headers.put("Accept-Language", TranslationFactory.getDesiredLanguage());
        headers.put("User-Agent", "AppWork " + Application.getApplication());
        headers.put("Accept-Charset", "UTF-8");
        headers.put("Connection", "Close");
        return headers;
    }

    private Map<String, String> parseHeadersJson(final String json) {
        final DataObject dataObject = Deser.fromString(json, new SimpleTypeRef<DataObject>(DataObject.class));
        final String headersJson = (String) dataObject.getData();
        final Map<String, String> headersMap = Deser.fromString(headersJson, new TypeRef<Map<String, String>>() {
        });
        return headersMap != null ? headersMap : new HashMap<String, String>();
    }

    private void verifyHeadersMatchExactly(final Map<String, String> expectedHeaders, final Map<String, String> actualHeaders, final String testName) throws Exception {
        this.verifyHeadersMatchExactly(expectedHeaders, actualHeaders, testName, null);
    }

    private void verifyHeadersMatchExactly(final Map<String, String> expectedHeaders, final Map<String, String> actualHeaders, final String testName, final RequestContext context) throws Exception {
        final Set<String> missingHeaders = new HashSet<String>();
        for (final Map.Entry<String, String> expected : expectedHeaders.entrySet()) {
            final String actualValue = actualHeaders.get(expected.getKey());
            if (actualValue == null) {
                missingHeaders.add(expected.getKey());
            } else if (!expected.getValue().equals(actualValue)) {
                final StringBuilder errorMsg = new StringBuilder();
                errorMsg.append(testName + ": Expected header '" + expected.getKey() + "' with value '" + expected.getValue() + "', but got '" + actualValue + "'");
                this.appendContextInfo(errorMsg, context);
                throw new AssertionError(errorMsg.toString());
            }
        }

        final Set<String> extraHeaders = new HashSet<String>();
        for (final String actualKey : actualHeaders.keySet()) {
            if (!expectedHeaders.containsKey(actualKey)) {
                extraHeaders.add(actualKey);
            }
        }

        if (!missingHeaders.isEmpty() || !extraHeaders.isEmpty()) {
            final StringBuilder errorMsg = new StringBuilder();
            errorMsg.append(testName + ": Header verification failed.\n");
            if (!missingHeaders.isEmpty()) {
                errorMsg.append("Missing headers: " + missingHeaders + "\n");
            }
            if (!extraHeaders.isEmpty()) {
                errorMsg.append("Extra headers: " + extraHeaders + "\n");
            }
            errorMsg.append("Expected headers: " + expectedHeaders + "\n");
            errorMsg.append("Actual headers: " + actualHeaders);
            this.appendContextInfo(errorMsg, context);
            throw new AssertionError(errorMsg.toString());
        }

        LogV3.info(testName + ": All " + expectedHeaders.size() + " expected headers verified correctly");
    }

    private void appendContextInfo(final StringBuilder errorMsg, final RequestContext context) throws IOException, InterruptedException {
        if (context != null) {
            errorMsg.append("\n\nHttpClient Context Information:\n");
            errorMsg.append("  URL: " + (context.getUrl() != null ? context.getUrl() : "null") + "\n");
            errorMsg.append("  Response Code: " + context.getCode() + "\n");
            errorMsg.append("  Method: " + (context.getMethod() != null ? context.getMethod() : "null") + "\n");
            if (context.getResponseString() != null) {
                final String responseBody = context.getResponseString();
                if (responseBody.length() > 500) {
                    errorMsg.append("  Response Body (first 500 chars): " + responseBody.substring(0, 500) + "...\n");
                } else {
                    errorMsg.append("  Response Body: " + responseBody + "\n");
                }
            }
            if (context.getConnection() != null) {
                errorMsg.append("  Connection: " + context.getConnection().getClass().getSimpleName() + "\n");
            }
        }
    }

    private Map<String, String> getResponseHeaders(final RequestContext context) {
        final Map<String, String> headers = new HashMap<String, String>();
        if (context == null || context.getConnection() == null) {
            return headers;
        }

        final java.util.Map<String, java.util.List<String>> headerFields = context.getConnection().getHeaderFields();
        if (headerFields != null) {
            for (final java.util.Map.Entry<String, java.util.List<String>> entry : headerFields.entrySet()) {
                final String key = entry.getKey();
                if (key != null) {
                    final java.util.List<String> values = entry.getValue();
                    if (values != null && !values.isEmpty()) {
                        headers.put(key, String.join(", ", values));
                    }
                }
            }
        }
        return headers;
    }

    private Map<String, String> getExpectedResponseHeaders(final Map<String, String> actualHeaders) {
        final Map<String, String> headers = new HashMap<String, String>();
        headers.put("Content-Type", "application/json");
        headers.put(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS, "nosniff");
        headers.put(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY, "frame-ancestors 'none'; default-src 'none'");
        headers.put(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY, "no-referrer");
        headers.put("Connection", "close");
        headers.put("Cache-Control", "no-store, no-cache");
        if (actualHeaders.containsKey("Server")) {
            headers.put("Server", actualHeaders.get("Server"));
        }
        if (actualHeaders.containsKey("Content-Length")) {
            headers.put("Content-Length", actualHeaders.get("Content-Length"));
        }
        return headers;
    }

    private String findHeaderIgnoreCase(final Map<String, String> headers, final String headerName) {
        for (final Map.Entry<String, String> entry : headers.entrySet()) {
            if (entry.getKey().equalsIgnoreCase(headerName)) {
                return entry.getValue();
            }
        }
        return null;
    }
}
