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

import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HeaderValidationRules;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.RequestSizeLimits;
import org.appwork.utils.net.httpserver.SecFetchDest;
import org.appwork.utils.net.httpserver.SecFetchMode;
import org.appwork.utils.net.httpserver.SecFetchSite;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Base class for HTTP server tests providing common setup and teardown functionality.
 *
 * <p>
 * This class provides shared infrastructure for HTTP server tests, including:
 * </p>
 * <ul>
 * <li>Server setup and teardown</li>
 * <li>HTTP client configuration</li>
 * <li>Common test utilities</li>
 * </ul>
 *
 * @author AppWork
 */
public abstract class HttpServerTestBase extends AWTest {
    /**
     * @author thomas
     * @date 21.01.2026
     *
     */
    final class TestHttpServer extends HttpServer {
        /**
         * @param port
         */
        private TestHttpServer(int port) {
            super(port);
        }

        public boolean onException(Throwable e, org.appwork.utils.net.httpserver.requests.HttpRequest request, org.appwork.utils.net.httpserver.responses.HttpResponse response) throws IOException {

            HttpServerTestBase.this.lastServerException = e;
            HttpServerTestBase.this.lastRequest = request;
            HttpServerTestBase.this.lastResponse = response;
            return super.onException(e, request, response);
        }
    }

    protected TestHttpServer httpServer;
    protected RemoteAPI      remoteAPI;
    protected int            serverPort;
    protected HttpClient     httpClient;
    protected Throwable      lastServerException;
    protected HttpRequest    lastRequest;
    protected HttpResponse   lastResponse;

    /**
     * Server Setup: Creates and starts an HTTP Server with Dummy API
     */
    protected void setupServer() throws IOException, ParseException {

        LogV3.info("re-warm IPv6 (isGlobalIPv6Available) availability check to cache result and avoid delays during tests...");
        // Pre-warm IPv6 availability check to cache result and avoid delays during tests
        HTTPConnectionUtils.isGlobalIPv6Available(5000, 5000, 60 * 60 * 1000L);
        LogV3.info("Starting HTTP Server Setup...");
        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Create HttpServer on a free port (0 = automatic)
        this.httpServer = new TestHttpServer(0);

        this.httpServer.setLocalhostOnly(true);

        // Register RemoteAPI as request handler
        this.httpServer.registerRequestHandler(this.remoteAPI);

        // Start server
        this.httpServer.start();
        this.serverPort = this.httpServer.getActualPort();

        // Note: Input stream draining is enabled by default via RequestSizeLimits.DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES

        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout(5000);
        this.httpClient.setReadTimeout(30000);
        // Set default mandatory header for all requests
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");

        LogV3.info("HTTP Server started on port: " + this.serverPort);
    }

    /**
     * Server Teardown: Stops the server
     */
    protected void teardownServer() {
        if (this.httpServer != null) {
            try {
                this.httpServer.shutdown();
                LogV3.info("HTTP Server stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Helper method to log context information when a test assertion fails.
     *
     * <p>
     * This method safely logs all available context information without throwing exceptions, ensuring that logging failures don't interfere
     * with test execution.
     * </p>
     *
     * @param context
     *            The RequestContext to log, or null
     * @param message
     *            Additional message to include in the log
     */
    protected void logContextOnFailure(final HttpClient.RequestContext context, final String message) {
        try {
            LogV3.severe("===========================================");
            LogV3.severe("TEST FAILED: " + message);
            LogV3.severe("===========================================");
            if (context != null) {
                LogV3.severe("Failed Request Context Details:");
                LogV3.severe("  Response Code: " + context.getCode());
                if (context.getConnection() != null) {
                    try {
                        final org.appwork.utils.net.httpconnection.HTTPConnection connection = context.getConnection();
                        LogV3.severe("  Request URL: " + connection.getURL());
                        LogV3.severe("  Request Method: " + context.getMethod());
                        LogV3.severe("  Response Headers:");
                        final java.util.Map<String, java.util.List<String>> headerFields = connection.getHeaderFields();
                        if (headerFields != null) {
                            for (final java.util.Map.Entry<String, java.util.List<String>> entry : headerFields.entrySet()) {
                                LogV3.severe("    " + entry.getKey() + ": " + entry.getValue());
                            }
                        }
                    } catch (final Throwable e2) {
                        LogV3.severe("  Error getting connection details: " + org.appwork.utils.Exceptions.getStackTrace(e2));
                    }
                }
                LogV3.severe("  Full Context: " + context.toString());
            } else {
                LogV3.severe("  RequestContext was null.");
            }
            LogV3.severe("===========================================");
        } catch (final Throwable e) {
            LogV3.severe("Error logging context: " + org.appwork.utils.Exceptions.getStackTrace(e));
        }
    }

    /**
     * Helper method to create an AssertionError with HttpClient context information included in the error message.
     *
     * <p>
     * This method appends HttpClient context details (URL, response code, method, response body) to the error message, making it easier to
     * debug test failures.
     * </p>
     *
     * @param message
     *            The base error message
     * @param context
     *            The RequestContext to include in the error message, or null
     * @return An AssertionError with the message and context information
     * @throws InterruptedException
     * @throws IOException
     */
    protected AssertionError createAssertionErrorWithContext(final String message, final HttpClient.RequestContext context) throws IOException, InterruptedException {
        final StringBuilder errorMsg = new StringBuilder(message);
        this.appendContextInfoToMessage(errorMsg, context);
        return new AssertionError(errorMsg.toString());
    }

    /**
     * Helper method to append HttpClient context information to an error message.
     *
     * @param errorMsg
     *            The StringBuilder to append context information to
     * @param context
     *            The RequestContext to include, or null
     * @throws InterruptedException
     * @throws IOException
     */
    protected void appendContextInfoToMessage(final StringBuilder errorMsg, final HttpClient.RequestContext context) throws IOException, InterruptedException {
        if (context != null) {
            errorMsg.append("\n\nHttpClient Context Information:\n");
            errorMsg.append("  URL: " + (context.getUrl() != null ? context.getUrl() : "null") + "\n");
            errorMsg.append("  Response Code: " + context.getCode() + "\n");
            errorMsg.append("  Method: " + (context.getMethod() != null ? context.getMethod() : "null") + "\n");
            final String responseBody = context.getResponseString();
            if (responseBody != null) {

                if (responseBody.length() > 500) {
                    errorMsg.append("  Response Body (first 500 chars): " + responseBody.substring(0, 500) + "...\n");
                } else {
                    errorMsg.append("  Response Body: " + responseBody + "\n");
                }
            }
            if (context.getConnection() != null) {
                try {
                    errorMsg.append("  Connection: " + context.getConnection().getClass().getSimpleName() + "\n");
                    final java.util.Map<String, java.util.List<String>> headerFields = context.getConnection().getHeaderFields();
                    if (headerFields != null && !headerFields.isEmpty()) {
                        errorMsg.append("  Response Headers:\n");
                        for (final java.util.Map.Entry<String, java.util.List<String>> entry : headerFields.entrySet()) {
                            final String key = entry.getKey();
                            if (key != null) {
                                errorMsg.append("    " + key + ": " + entry.getValue() + "\n");
                            }
                        }
                    }
                } catch (final Throwable e) {
                    errorMsg.append("  Error getting connection details: " + org.appwork.utils.Exceptions.getStackTrace(e) + "\n");
                }
            }
        }
    }

    /**
     * Server Setup with Size Limits: Creates and starts an HTTP Server with Dummy API and size limits
     */
    protected void setupServerWithLimits(final int maxHeaderSize, final long maxPostBodySize) throws IOException, ParseException {
        this.setupServerWithLimits(maxHeaderSize, maxPostBodySize, -1);
    }

    /**
     * Server Setup with Size Limits: Creates and starts an HTTP Server with Dummy API and size limits
     */
    protected void setupServerWithLimits(final int maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize) throws IOException, ParseException {
        LogV3.info("Starting HTTP Server Setup with Limits (maxHeaderSize=" + maxHeaderSize + ", maxPostBodySize=" + maxPostBodySize + ", maxPostProcessedSize=" + maxPostProcessedSize + ")...");

        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Create HttpServer on a free port (0 = automatic) with exception tracking
        this.httpServer = new TestHttpServer(0);

        this.httpServer.setLocalhostOnly(true);

        // Set request size limits BEFORE registering handlers and starting
        final RequestSizeLimits limits = new RequestSizeLimits(maxHeaderSize, maxPostBodySize, maxPostProcessedSize);
        this.httpServer.setRequestSizeLimits(limits);

        // Register RemoteAPI as request handler
        this.httpServer.registerRequestHandler(this.remoteAPI);

        // Start server
        this.httpServer.start();
        this.serverPort = this.httpServer.getActualPort();

        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout(5000);
        this.httpClient.setReadTimeout(30000);
        // Set default mandatory header for all requests
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");

        LogV3.info("HTTP Server started on port: " + this.serverPort + " with size limits");
    }

    /**
     * Server Setup with Browser Header Rules: Creates and starts an HTTP Server with header validation rules that allow direct browser
     * requests (sec-fetch-* headers allowed, no mandatory x-appwork header)
     */
    protected void setupServerWithBrowserHeaders() throws IOException, ParseException {
        LogV3.info("Starting HTTP Server Setup with Browser Header Rules...");

        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Create HttpServer on a free port (0 = automatic) with exception tracking
        this.httpServer = new TestHttpServer(0);

        this.httpServer.setLocalhostOnly(true);

        // Set header validation rules that allow direct browser requests BEFORE starting
        // - No mandatory headers (allow direct browser navigation)
        // - No forbidden headers (allow sec-fetch-* headers)
        final Map<String, String> mandatoryHeaders = new HashMap<String, String>();
        final Map<String, String> forbiddenHeaders = new HashMap<String, String>();

        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, SecFetchSite.NONE.getValue());
        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, SecFetchMode.NAVIGATE.getValue());
        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, SecFetchDest.DOCUMENT.getValue());
        final HeaderValidationRules rules = new HeaderValidationRules(mandatoryHeaders, forbiddenHeaders);
        this.httpServer.setHeaderValidationRules(rules);

        // Register RemoteAPI as request handler
        this.httpServer.registerRequestHandler(this.remoteAPI);

        // Start server
        this.httpServer.start();
        this.serverPort = this.httpServer.getActualPort();

        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout(5000);
        this.httpClient.setReadTimeout(30000);

        LogV3.info("HTTP Server started on port: " + this.serverPort + " with browser header rules");
    }

    /**
     * Helper method to temporarily allow HTTP methods for testing.
     *
     * @param methods
     *            The HTTP methods to allow
     * @return The previous set of allowed methods
     */
    protected Set<RequestMethod> allowHttpMethods(final RequestMethod... methods) {
        final Set<RequestMethod> previousMethods = this.httpServer.getAllowedMethods();
        final EnumSet<RequestMethod> newMethods = EnumSet.noneOf(RequestMethod.class);
        for (final RequestMethod method : methods) {
            newMethods.add(method);
        }
        this.httpServer.setAllowedMethods(newMethods);
        return previousMethods;
    }

    /**
     * Helper method to restore HTTP methods.
     *
     * @param methods
     *            The HTTP methods to restore
     */
    protected void restoreHttpMethods(final Set<RequestMethod> methods) {
        if (methods != null) {
            this.httpServer.setAllowedMethods(methods);
        } else {
            this.httpServer.setAllowedMethods(EnumSet.of(RequestMethod.GET));
        }
    }

    /**
     * Assertion helper that includes HttpClient context information in the error message if the assertion fails.
     *
     * @param condition
     *            The condition to assert (must be true)
     * @param message
     *            The error message if assertion fails
     * @param context
     *            The RequestContext to include in error message, or null
     * @throws InterruptedException
     * @throws IOException
     * @throws AssertionError
     */
    protected void assertTrueWithContext(final boolean condition, final String message, final HttpClient.RequestContext context) throws AssertionError, IOException, InterruptedException {
        if (!condition) {
            this.logContextOnFailure(context, message);
            throw this.createAssertionErrorWithContext(message, context);
        }
    }
}
