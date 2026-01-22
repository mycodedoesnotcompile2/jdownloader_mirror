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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.EnumSet;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.requests.CopyRequest;
import org.appwork.utils.net.httpserver.requests.DeleteRequest;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.LockRequest;
import org.appwork.utils.net.httpserver.requests.MkcolRequest;
import org.appwork.utils.net.httpserver.requests.MoveRequest;
import org.appwork.utils.net.httpserver.requests.MsearchRequest;
import org.appwork.utils.net.httpserver.requests.NotifyRequest;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.requests.PatchRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.requests.PropfindRequest;
import org.appwork.utils.net.httpserver.requests.ProppatchRequest;
import org.appwork.utils.net.httpserver.requests.PutRequest;
import org.appwork.utils.net.httpserver.requests.SubscribeRequest;
import org.appwork.utils.net.httpserver.requests.TraceRequest;
import org.appwork.utils.net.httpserver.requests.UnlockRequest;
import org.appwork.utils.net.httpserver.requests.UnsubscribeRequest;

/**
 * Tests for HTTP server method validation functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Dangerous HTTP methods (CONNECT, TRACE, PUT, DELETE, OPTIONS, HEAD, POST) are blocked by default</li>
 * <li>Only GET is allowed by default</li>
 * <li>Methods can be explicitly allowed via setAllowedMethods</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerMethodValidationTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testDangerousHttpMethodsBlocked();
            this.testPreflightOptionsBlockedByDefault();
            this.testApiMethodHttpMethodValidation();
            this.testExplicitMethodAllowance();
            this.testAllMethodsAllowedWhenNull();
            this.testGetRequestWithBodyData();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Dangerous HTTP methods should be blocked by default
     *
     * Tests that all methods except GET are blocked by default. Only GET should be allowed by default. Other methods must be explicitly
     * allowed via setAllowedMethods.
     */
    private void testDangerousHttpMethodsBlocked() throws Exception {
        LogV3.info("Test: Dangerous HTTP Methods Blocked");

        // Test that GET is allowed (default)
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        RequestContext context = this.httpClient.get(url);
        final int getResponseCode = context.getCode();
        assertTrue(getResponseCode == 200, "GET request should return 200, was: " + getResponseCode);
        // Verify Request class is included in response
        final String responseBody = context.getResponseString();
        assertTrue(responseBody.contains("[Request: " + GetRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + GetRequest.class.getSimpleName() + "]', was: " + responseBody);
        LogV3.info("GET method test passed: " + getResponseCode + ", response: " + responseBody);

        // Test all other RequestMethods are blocked by default
        final EnumSet<RequestMethod> allMethods = EnumSet.allOf(RequestMethod.class);
        allMethods.remove(RequestMethod.GET); // GET is allowed by default
        allMethods.remove(RequestMethod.UNKNOWN); // UNKNOWN is not a real HTTP method

        for (final RequestMethod method : allMethods) {
            this.testMethodBlockedByDefault(method, url);
        }

        // Test that POST can be explicitly allowed
        LogV3.info("Test: POST can be explicitly allowed");
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);

        try {
            final String postUrl = "http://localhost:" + this.serverPort + "/test/postData";
            final byte[] postData = "{\"params\":[\"test\"]}".getBytes("UTF-8");
            final RequestContext requestContext = new RequestContext().setMethod(RequestMethod.POST).setUrl(postUrl).setPostDataStream(new ByteArrayInputStream(postData)).setPostDataLength(postData.length);
            requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            context = this.httpClient.execute(requestContext);
            final int postAllowedResponseCode = context.getCode();
            assertTrue(postAllowedResponseCode == 200, "POST request should return 200 after being explicitly allowed, was: " + postAllowedResponseCode);
            // Verify Request class is included in response
            final String postResponseBody = context.getResponseString();
            assertTrue(postResponseBody.contains("[Request: " + PostRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + PostRequest.class.getSimpleName() + "]', was: " + postResponseBody);
            LogV3.info("POST method allowed test passed: " + postAllowedResponseCode + ", response: " + postResponseBody);
        } finally {
            this.restoreHttpMethods(previousMethods);
        }

        LogV3.info("Dangerous HTTP Methods Blocked test completed successfully!");
    }

    /**
     * Test: Preflight OPTIONS request should be blocked by default when CORS is disabled
     */
    private void testPreflightOptionsBlockedByDefault() throws Exception {
        LogV3.info("Test: Preflight OPTIONS blocked by default (CORS disabled)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

        final RequestContext context = this.httpClient.execute(new RequestContext().setMethod(RequestMethod.OPTIONS).setUrl(url).addHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://example.com").addHeader("Access-Control-Request-Method", RequestMethod.GET.name()));
        final int responseCode = context.getCode();
        assertTrue(responseCode == ResponseCode.METHOD_NOT_ALLOWED.getCode(), "Preflight OPTIONS should return " + ResponseCode.METHOD_NOT_ALLOWED.getCode() + ", was: " + responseCode);
        LogV3.info("Preflight OPTIONS blocked test passed: " + responseCode);
    }

    /**
     * Test: API methods validate HTTP method
     *
     * Tests that API methods reject requests with wrong HTTP methods (e.g., POST to /test/echo should fail, GET to /test/postData should
     * fail).
     */
    private void testApiMethodHttpMethodValidation() throws Exception {
        LogV3.info("Test: API Method HTTP Method Validation");

        // Test that POST to /test/echo is rejected (echo only accepts GET)
        LogV3.info("Test: POST to /test/echo should be rejected");
        final String echoUrl = "http://localhost:" + this.serverPort + "/test/echo";

        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
        try {
            final byte[] postData = "{\"params\":[\"test\"]}".getBytes("UTF-8");
            final RequestContext requestContext = new RequestContext().setMethod(RequestMethod.POST).setUrl(echoUrl).setPostDataStream(new ByteArrayInputStream(postData)).setPostDataLength(postData.length);
            requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            final RequestContext context = this.httpClient.execute(requestContext);
            final int responseCode = context.getCode();
            // POST to /test/echo should return 400 (Bad Request) because echo only accepts GetRequest
            assertTrue(responseCode == 400, "POST to /test/echo should return 400 (Bad Request), was: " + responseCode);
            final String responseBodyPost = context.getResponseString();
            assertTrue(responseBodyPost.contains("not allowed") || responseBodyPost.contains("PostRequest"), "Response should indicate that PostRequest is not allowed, was: " + responseBodyPost);
            LogV3.info("Test passed: POST to /test/echo correctly rejected with " + responseCode);
        } finally {
            this.restoreHttpMethods(previousMethods);
        }

        // Test that GET to /test/postData is rejected (postData only accepts POST)
        LogV3.info("Test: GET to /test/postData should be rejected");
        final String postDataUrl = "http://localhost:" + this.serverPort + "/test/postData?data";
        final Set<RequestMethod> previousMethods2 = this.allowHttpMethods(RequestMethod.GET, RequestMethod.POST);
        try {
            final RequestContext requestContext = new RequestContext().setMethod(RequestMethod.GET).setUrl(postDataUrl);
            final RequestContext context = this.httpClient.execute(requestContext);
            final int responseCode = context.getCode();
            // GET to /test/postData should return 400 (Bad Request) because postData only accepts PostRequest
            assertTrue(responseCode == 400, "GET to /test/postData should return 400 (Bad Request), was: " + responseCode);
            final String responseBodyGet = context.getResponseString();
            assertTrue(responseBodyGet.contains("not allowed") || responseBodyGet.contains("GetRequest"), "Response should indicate that GetRequest is not allowed, was: " + responseBodyGet);
            LogV3.info("Test passed: GET to /test/postData correctly rejected with " + responseCode);
        } finally {
            this.restoreHttpMethods(previousMethods2);
        }

        LogV3.info("API Method HTTP Method Validation test completed successfully!");
    }

    /**
     * Helper method to test that a method is blocked by default
     */
    private void testMethodBlockedByDefault(final RequestMethod method, final String url) throws Exception {
        final String postUrl = "http://localhost:" + this.serverPort + "/test/postData";
        RequestContext context = null;
        if (method.mayHavePostBody) {
            // Methods that require output stream
            final byte[] data = "{\"params\":[\"test\"]}".getBytes("UTF-8");
            final String targetUrl = postUrl;
            final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(targetUrl).setPostDataStream(new ByteArrayInputStream(data)).setPostDataLength(data.length);
            requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            context = this.httpClient.execute(requestContext);
        } else {
            // Methods that don't require output stream
            final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(url);
            context = this.httpClient.execute(requestContext);
        }
        final int responseCode = context.getCode();
        assertTrue(responseCode == ResponseCode.METHOD_NOT_ALLOWED.getCode(), method.name() + " request should return " + ResponseCode.METHOD_NOT_ALLOWED.getCode() + ", was: " + responseCode);
        LogV3.info(method.name() + " method blocked test passed: " + responseCode);
    }

    /**
     * Test: Various HTTP methods can be explicitly allowed via setAllowedMethods
     *
     * Tests that all RequestMethods can be explicitly allowed.
     */
    private void testExplicitMethodAllowance() throws Exception {
        LogV3.info("Test: Explicit Method Allowance");

        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String postUrl = "http://localhost:" + this.serverPort + "/test/postData";

        // Test all RequestMethods can be explicitly allowed
        final EnumSet<RequestMethod> allMethods = EnumSet.allOf(RequestMethod.class);
        allMethods.remove(RequestMethod.GET); // GET is already tested
        allMethods.remove(RequestMethod.UNKNOWN); // UNKNOWN is not a real HTTP method

        for (final RequestMethod method : allMethods) {
            this.testMethodCanBeExplicitlyAllowed(method, url, postUrl);
        }

        LogV3.info("Explicit Method Allowance test completed successfully!");
    }

    /**
     * Helper method to test that a method can be explicitly allowed
     */
    private void testMethodCanBeExplicitlyAllowed(final RequestMethod method, final String url, final String postUrl) throws Exception {
        LogV3.info("Test: " + method.name() + " can be explicitly allowed");
        final Set<RequestMethod> previousMethods = this.allowHttpMethods(RequestMethod.GET, method);

        try {
            RequestContext context = null;
            if (method.mayHavePostBody) {
                // Methods that require output stream
                final byte[] data = "{\"params\":[\"test\"]}".getBytes("UTF-8");
                final String targetUrl = postUrl;
                final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(targetUrl).setPostDataStream(new ByteArrayInputStream(data)).setPostDataLength(data.length);

                requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");

                context = this.httpClient.execute(requestContext);
            } else {
                // Methods that don't require output stream
                final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(url);
                context = this.httpClient.execute(requestContext);
            }
            final String responseBody = context.getResponseString();
            final int responseCode = context.getCode();
            // If method is explicitly allowed, it should not return 405 (METHOD_NOT_ALLOWED)
            // POST to /test/postData should return 200 (handler supports it)
            // Other methods should return 501 (NOT_IMPLEMENTED) if handler doesn't support them, not 405
            if (method == RequestMethod.CONNECT) {

                assertTrue(responseCode == ResponseCode.METHOD_NOT_ALLOWED.getCode(), "Connect is expected to be 405 here, because it is used for proxy servers only!, was: " + responseCode);
                return;
            }

            assertTrue(responseCode == 200, method.name() + " request to " + context.getUrl() + " should return 200, was: " + responseCode);
            // Verify Request class is included in response

            if (method == RequestMethod.GET) {
                assertTrue(responseBody.contains("[Request: " + GetRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + GetRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.POST) {
                assertTrue(responseBody.contains("[Request: " + PostRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + PostRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.PUT) {
                assertTrue(responseBody.contains("[Request: " + PutRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + PutRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.DELETE) {
                assertTrue(responseBody.contains("[Request: " + DeleteRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + DeleteRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.HEAD) {

                assertTrue(responseBody.equals(""), "No response for head request!. was: " + responseBody);
            } else if (method == RequestMethod.OPTIONS) {
                assertTrue(responseBody.contains("[Request: " + OptionsRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + OptionsRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.TRACE) {
                assertTrue(responseBody.contains("[Request: " + TraceRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + TraceRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.CONNECT) {
                throw new WTFException();
            } else if (method == RequestMethod.PATCH) {
                assertTrue(responseBody.contains("[Request: " + PatchRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + PatchRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.COPY) {
                assertTrue(responseBody.contains("[Request: " + CopyRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + CopyRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.MOVE) {
                assertTrue(responseBody.contains("[Request: " + MoveRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + MoveRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.LOCK) {
                assertTrue(responseBody.contains("[Request: " + LockRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + LockRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.UNLOCK) {
                assertTrue(responseBody.contains("[Request: " + UnlockRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + UnlockRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.MKCOL) {
                assertTrue(responseBody.contains("[Request: " + MkcolRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + MkcolRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.PROPFIND) {
                assertTrue(responseBody.contains("[Request: " + PropfindRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + PropfindRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.PROPPATCH) {
                assertTrue(responseBody.contains("[Request: " + ProppatchRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + ProppatchRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.SUBSCRIBE) {
                assertTrue(responseBody.contains("[Request: " + SubscribeRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + SubscribeRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.UNSUBSCRIBE) {
                assertTrue(responseBody.contains("[Request: " + UnsubscribeRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + UnsubscribeRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.NOTIFY) {
                assertTrue(responseBody.contains("[Request: " + NotifyRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + NotifyRequest.class.getSimpleName() + "]', was: " + responseBody);
            } else if (method == RequestMethod.MSEARCH) {
                assertTrue(responseBody.contains("[Request: " + MsearchRequest.class.getSimpleName() + "]"), "Response should contain '[Request: " + MsearchRequest.class.getSimpleName() + "]', was: " + responseBody);
            }

            LogV3.info(method.name() + " method allowed test passed: " + responseCode);
        } finally {
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Test: All HTTP methods are allowed when setAllowedMethods(null) is set
     *
     * Tests that when allowed methods is set to null, all HTTP methods are allowed (method validation is disabled).
     */
    private void testAllMethodsAllowedWhenNull() throws Exception {
        LogV3.info("Test: All Methods Allowed When Null");

        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final String postUrl = "http://localhost:" + this.serverPort + "/test/postData";

        // Save previous allowed methods
        final Set<RequestMethod> previousMethods = this.httpServer.getAllowedMethods();

        try {
            // Set allowed methods to null (disables method validation)
            this.httpServer.setAllowedMethods((Set) null);
            assertTrue(this.httpServer.getAllowedMethods() == null, "getAllowedMethods() should return null after setAllowedMethods(null)");

            // Test all RequestMethods work when methods are null
            final EnumSet<RequestMethod> allMethods = EnumSet.allOf(RequestMethod.class);
            allMethods.remove(RequestMethod.UNKNOWN); // UNKNOWN is not a real HTTP method
            allMethods.remove(RequestMethod.CONNECT); // UNKNOWN is for proxies only

            for (final RequestMethod method : allMethods) {
                this.testMethodWorksWhenNull(method, url, postUrl);
            }

            LogV3.info("All Methods Allowed When Null test completed successfully!");
        } finally {
            // Restore previous allowed methods
            this.restoreHttpMethods(previousMethods);
        }
    }

    /**
     * Helper method to test that a method works when allowed methods is null
     */
    private void testMethodWorksWhenNull(final RequestMethod method, final String url, final String postUrl) throws Exception {
        RequestContext context = null;
        if (method.mayHavePostBody) {
            // Methods that require output stream
            final byte[] data = "{\"params\":[\"test\"]}".getBytes("UTF-8");
            final String targetUrl = postUrl;
            final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(targetUrl).setPostDataStream(new ByteArrayInputStream(data)).setPostDataLength(data.length);
            requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            context = this.httpClient.execute(requestContext);
        } else {
            // Methods that don't require output stream
            final RequestContext requestContext = new RequestContext().setMethod(method).setUrl(url);
            context = this.httpClient.execute(requestContext);
        }
        final int responseCode = context.getCode();
        final String responseBody = context.getResponseString();
        // When methods is null, all methods are allowed, so they should not return 405 (METHOD_NOT_ALLOWED)
        // GET to /test/echo should return 200, POST to /test/postData should return 200
        // Other methods should return 501 (NOT_IMPLEMENTED) if handler doesn't support them, not 405
        if (method == RequestMethod.GET) {
            assertTrue(responseCode == 200, method.name() + " request to /test/echo should return 200, was: " + responseCode);
            // Verify Request class is included in response
            assertTrue(responseBody.contains(" [Request: " + GetRequest.class.getSimpleName() + "]"), "Response should contain ' [Request: " + GetRequest.class.getSimpleName() + "]', was: " + responseBody);
        } else if (method == RequestMethod.POST) {
            assertTrue(responseCode == 200, method.name() + " request to /test/postData should return 200, was: " + responseCode);
            // Verify Request class is included in response
            assertTrue(responseBody.contains(" [Request: " + PostRequest.class.getSimpleName() + "]"), "Response should contain ' [Request: " + PostRequest.class.getSimpleName() + "]', was: " + responseBody);
        } else {
            // Method is allowed (methods is null), so it should work fine
            assertTrue(responseCode == ResponseCode.SUCCESS_OK.getCode(), method.name() + " request should not return 405 (METHOD_NOT_ALLOWED) when methods is null (all allowed), was: " + responseCode);
        }
        LogV3.info(method.name() + " method test passed (null allowed): " + responseCode);
    }

    /**
     * Test: GET request with body data should be handled correctly
     *
     * Tests that the server responds correctly when a GET request is made but body data is still sent. According to HTTP specification, GET
     * requests should not have a body, but the server should handle this edge case gracefully (either ignore the body or return an
     * appropriate response).
     */
    private void testGetRequestWithBodyData() throws Exception {
        LogV3.info("Test: GET Request with Body Data");

        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        final byte[] bodyData = "This is body data that should not be sent with GET".getBytes("UTF-8");

        // Create a GET request with body data
        final RequestContext requestContext = new RequestContext().setMethod(RequestMethod.GET).setUrl(url).setPostDataStream(new ByteArrayInputStream(bodyData)).setPostDataLength(bodyData.length);
        requestContext.addHeader(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "text/plain");

        RequestContext context = null;
        try {
            // fake get with post data
            context = new HttpClient() {
                {
                    putRequestHeader(HTTPConstants.X_APPWORK, "1");
                }

                protected org.appwork.utils.net.httpconnection.HTTPConnection createHTTPConnection(RequestContext context) throws HttpClientException {

                    try {
                        HTTPConnection ret = new HTTPConnectionImpl(new URL(context.getUrl()), null) {
                            protected boolean isRequiresOutputStream() {
                                return true;
                            };
                        };
                        context.setConnection(ret);
                        return ret;
                    } catch (MalformedURLException e) {
                        throw new WTFException(e);
                    }

                };
            }.execute(requestContext);

            final int responseCode = context.getCode();
            // The server should handle GET requests with body data gracefully and return 200 (ignoring the body)
            assertTrue(responseCode == 200, "GET request with body data should return 200 (ignoring body), was: " + responseCode);
            LogV3.info("GET request with body data test passed: " + responseCode);
        } catch (final HttpClientException e) {
            e.printStackTrace();
            throw e;
        }

        LogV3.info("GET Request with Body Data test completed successfully!");
    }
}
