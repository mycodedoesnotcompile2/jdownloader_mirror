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

import java.net.URLEncoder;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpserver.SecFetchDest;
import org.appwork.utils.net.httpserver.SecFetchMode;
import org.appwork.utils.net.httpserver.SecFetchSite;
import org.appwork.utils.net.httpserver.SecFetchUser;
import org.appwork.utils.os.CrossSystem;

/**
 * Tests for HTTP server header validation functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Mandatory headers are enforced</li>
 * <li>Forbidden headers are rejected</li>
 * <li>Browser requests work correctly with appropriate header rules</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerHeaderValidationTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testDefaultMandatoryHeaderPresent();
            this.testDefaultMandatoryHeaderMissing();
            this.testDefaultMandatoryHeaderWrongValue();
            this.testDefaultForbiddenHeaderSecFetchSite();
            this.testDefaultForbiddenHeaderSecFetchMode();
            this.testDefaultForbiddenHeaderSecFetchUser();
            this.testDefaultForbiddenHeaderSecFetchDest();
            this.testDefaultForbiddenHeaderAbsent();
            this.testDirectBrowserRequest();
            this.testRealBrowserRequest();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test: Default mandatory header (x-appwork: 1) present with correct value should succeed
     */
    private void testDefaultMandatoryHeaderPresent() throws Exception {
        LogV3.info("Test: Default Mandatory Header Present (x-appwork: 1)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default mandatory header: x-appwork: 1
        // x-appwork header is already set in setupServer()
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        // Request with default mandatory header should succeed
        assertTrue(responseCode == 200, "Request with default mandatory header (x-appwork: 1) should return 200, was: " + responseCode);
        LogV3.info("Default Mandatory Header Present test successful: " + responseCode);
    }

    /**
     * Test: Default mandatory header missing should be rejected with 403 Forbidden
     */
    private void testDefaultMandatoryHeaderMissing() throws Exception {
        LogV3.info("Test: Default Mandatory Header Missing (x-appwork)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Remove x-appwork header - should be rejected
        this.httpClient.clearRequestHeader();
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request without default mandatory header must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request without default mandatory header (x-appwork) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Mandatory Header Missing test successful: " + responseCode);
        } finally {
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default mandatory header with wrong value should be rejected with 403 Forbidden
     */
    private void testDefaultMandatoryHeaderWrongValue() throws Exception {
        LogV3.info("Test: Default Mandatory Header Wrong Value (x-appwork)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default mandatory header: x-appwork: 1, but we send wrong value
        this.httpClient.clearRequestHeader();
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "2"); // Wrong value
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request with wrong default mandatory header value must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request with wrong default mandatory header value (x-appwork: 2) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Mandatory Header Wrong Value test successful: " + responseCode);
        } finally {
            this.httpClient.clearRequestHeader();
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default forbidden header sec-fetch-site present should be rejected with 403 Forbidden
     */
    private void testDefaultForbiddenHeaderSecFetchSite() throws Exception {
        LogV3.info("Test: Default Forbidden Header Present (sec-fetch-site)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default forbidden header: sec-fetch-site
        // x-appwork header is already set in setupServer()
        this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, SecFetchSite.SAME_ORIGIN.getValue()); // Set forbidden
                                                                                                                            // header
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request with default forbidden header must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request with default forbidden header (sec-fetch-site) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Forbidden Header (sec-fetch-site) test successful: " + responseCode);
        } finally {
            this.httpClient.clearRequestHeader();
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default forbidden header sec-fetch-mode present should be rejected with 403 Forbidden
     */
    private void testDefaultForbiddenHeaderSecFetchMode() throws Exception {
        LogV3.info("Test: Default Forbidden Header Present (sec-fetch-mode)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default forbidden header: sec-fetch-mode
        // x-appwork header is already set in setupServer()
        this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, SecFetchMode.NAVIGATE.getValue()); // Set forbidden
                                                                                                                         // header
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request with default forbidden header must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request with default forbidden header (sec-fetch-mode) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Forbidden Header (sec-fetch-mode) test successful: " + responseCode);
        } finally {
            this.httpClient.clearRequestHeader();
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default forbidden header sec-fetch-user present should be rejected with 403 Forbidden
     */
    private void testDefaultForbiddenHeaderSecFetchUser() throws Exception {
        LogV3.info("Test: Default Forbidden Header Present (sec-fetch-user)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default forbidden header: sec-fetch-user
        // x-appwork header is already set in setupServer()
        this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_USER, SecFetchUser.USER_INITIATED.getValue()); // Set
                                                                                                                               // forbidden
                                                                                                                               // header
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request with default forbidden header must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request with default forbidden header (sec-fetch-user) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Forbidden Header (sec-fetch-user) test successful: " + responseCode);
        } finally {
            this.httpClient.clearRequestHeader();
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default forbidden header sec-fetch-dest present should be rejected with 403 Forbidden
     */
    private void testDefaultForbiddenHeaderSecFetchDest() throws Exception {
        LogV3.info("Test: Default Forbidden Header Present (sec-fetch-dest)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default forbidden header: sec-fetch-dest
        // x-appwork header is already set in setupServer()
        this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, SecFetchDest.DOCUMENT.getValue()); // Set forbidden
                                                                                                                         // header
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // Request with default forbidden header must return 403 Forbidden
            assertTrue(responseCode == ResponseCode.ERROR_BAD_REQUEST.getCode(), "Request with default forbidden header (sec-fetch-dest) must return " + ResponseCode.ERROR_BAD_REQUEST.getCode() + " Forbidden, was: " + responseCode);
            LogV3.info("Default Forbidden Header (sec-fetch-dest) test successful: " + responseCode);
        } finally {
            this.httpClient.clearRequestHeader();
            // Restore mandatory header after clearing
            this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        }
    }

    /**
     * Test: Default forbidden headers absent should succeed (if mandatory headers are present)
     */
    private void testDefaultForbiddenHeaderAbsent() throws Exception {
        LogV3.info("Test: Default Forbidden Headers Absent");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";
        // Server has default forbidden headers: sec-fetch-* headers, but we don't send them
        // x-appwork header is already set in setupServer()
        // Don't set any sec-fetch-* headers - should succeed
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        // Request without default forbidden headers should succeed (if mandatory headers are present)
        assertTrue(responseCode == 200, "Request without default forbidden headers should return 200, was: " + responseCode);
        LogV3.info("Default Forbidden Headers Absent test successful: " + responseCode);
    }

    /**
     * Test: Direct browser requests should work when header validation rules are configured appropriately.
     *
     * When a user types a URL directly in the browser, the browser sends sec-fetch-* headers like: - sec-fetch-site: none - sec-fetch-mode:
     * navigate - sec-fetch-user: ?1 - sec-fetch-dest: document
     *
     * To allow direct browser navigation, we need to: 1. Remove sec-fetch-* headers from forbidden headers 2. Optionally allow
     * sec-fetch-site: none as mandatory (or just not forbid it)
     */
    private void testDirectBrowserRequest() throws Exception {
        LogV3.info("Test: Direct Browser Request");

        // Setup server with header validation rules that allow direct browser requests
        this.teardownServer();
        this.setupServerWithBrowserHeaders();

        try {
            final String url = "http://localhost:" + this.serverPort + "/test/echo?message=test";

            // Simulate direct browser request with sec-fetch-* headers
            // Note: We don't set x-appwork header because we're testing with custom rules
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, SecFetchSite.NONE.getValue());
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, SecFetchMode.NAVIGATE.getValue());
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_USER, SecFetchUser.USER_INITIATED.getValue());
            this.httpClient.putRequestHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, SecFetchDest.DOCUMENT.getValue());

            try {
                final RequestContext context = this.httpClient.get(url);
                final int responseCode = context.getCode();
                // Direct browser request should succeed when header rules allow it
                assertTrue(responseCode == 200, "Direct browser request should return 200, was: " + responseCode);
                LogV3.info("Direct Browser Request test successful: " + responseCode);
            } finally {
                this.httpClient.clearRequestHeader();
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }

    /**
     * Test: Real browser request - opens system browser and waits for the request
     *
     * This test opens the system browser with a URL and waits for the browser to make the request. The server should handle the browser
     * request correctly (with browser headers like sec-fetch-*).
     */
    private void testRealBrowserRequest() throws Exception {
        LogV3.info("Test: Real Browser Request");

        // Setup server with browser header rules (allow sec-fetch-* headers, no mandatory x-appwork)
        this.teardownServer();
        this.setupServerWithBrowserHeaders();

        try {
            // Generate unique test ID
            final String testId = UUID.randomUUID().toString();
            final String url = "http://localhost:" + this.serverPort + "/test/browserTest?testId=" + URLEncoder.encode(testId, "UTF-8");

            // Use CountDownLatch to wait for browser request
            final CountDownLatch latch = new CountDownLatch(1);

            // Start a thread that polls the endpoint to check if browser made the request
            final Thread pollThread = new Thread("BrowserTestPoll") {
                @Override
                public void run() {
                    try {
                        // Wait a bit for browser to open
                        Thread.sleep(2000);
                        if (StringUtils.equals(DummyTestAPIImpl.RECEIVED_TEST, testId)) {
                            latch.countDown();
                        }
                    } catch (final Exception e) {
                        LogV3.log(e);
                    }
                }
            };
            pollThread.start();

            // Open browser with the URL
            LogV3.info("Opening system browser with URL: " + url);
            final Throwable browserError = CrossSystem.openURL(url);
            if (browserError != null) {
                LogV3.warning("Failed to open browser: " + browserError.getMessage());
                // Continue anyway - browser might have opened
            }

            // Wait for browser request (with timeout)
            final boolean received = latch.await(10, TimeUnit.SECONDS);

            if (received) {
                LogV3.info("Real Browser Request test successful");
            } else {
                LogV3.warning("Real Browser Request test: Timeout waiting for browser request");
                throw new WTFException("DID not receiv Real Browser request");
            }
        } finally {
            this.teardownServer();
            this.setupServer(); // Restore server with default limits for other tests
        }
    }
}
