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
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HeaderValidationRules;
import org.appwork.utils.net.httpserver.HttpHandlerInfo;
import org.appwork.utils.net.httpserver.HttpServerController;
import org.appwork.utils.net.httpserver.RequestSizeLimits;
import org.appwork.utils.net.httpserver.SecFetchDest;
import org.appwork.utils.net.httpserver.SecFetchMode;
import org.appwork.utils.net.httpserver.SecFetchSite;

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
    protected HttpServerController serverController;
    protected RemoteAPI            remoteAPI;
    protected HttpHandlerInfo      handlerInfo;
    protected int                  serverPort;
    protected HttpClient           httpClient;

    /**
     * Server Setup: Creates and starts an HTTP Server with Dummy API
     */
    protected void setupServer() throws IOException, ParseException {
        LogV3.info("Starting HTTP Server Setup...");
        this.serverController = new HttpServerController();
        this.remoteAPI = new RemoteAPI();

        // Register Dummy API
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Start server on a free port (0 = automatic)
        this.handlerInfo = this.serverController.registerRequestHandler(0, true, this.remoteAPI);
        this.serverPort = this.handlerInfo.getPort();

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
        if (this.handlerInfo != null) {
            try {
                this.handlerInfo.unregisterRequestHandler();
                this.handlerInfo.getHttpServer().shutdown();
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
        this.serverController = new HttpServerController();
        this.remoteAPI = new RemoteAPI();

        // Register Dummy API
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Start server on a free port (0 = automatic)
        this.handlerInfo = this.serverController.registerRequestHandler(0, true, this.remoteAPI);
        this.serverPort = this.handlerInfo.getPort();

        // Set request size limits
        final RequestSizeLimits limits = new RequestSizeLimits(maxHeaderSize, maxPostBodySize, maxPostProcessedSize);
        this.handlerInfo.getHttpServer().setRequestSizeLimits(limits);

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
        this.serverController = new HttpServerController();
        this.remoteAPI = new RemoteAPI();

        // Register Dummy API
        this.remoteAPI.register(new DummyTestAPIImpl());

        // Start server on a free port (0 = automatic)
        this.handlerInfo = this.serverController.registerRequestHandler(0, true, this.remoteAPI);
        this.serverPort = this.handlerInfo.getPort();

        // Set header validation rules that allow direct browser requests
        // - No mandatory headers (allow direct browser navigation)
        // - No forbidden headers (allow sec-fetch-* headers)
        final Map<String, String> mandatoryHeaders = new HashMap<String, String>();
        final Map<String, String> forbiddenHeaders = new HashMap<String, String>();

        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, SecFetchSite.NONE.getValue());
        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, SecFetchMode.NAVIGATE.getValue());
        mandatoryHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, SecFetchDest.DOCUMENT.getValue());
        final HeaderValidationRules rules = new HeaderValidationRules(mandatoryHeaders, forbiddenHeaders);
        this.handlerInfo.getHttpServer().setHeaderValidationRules(rules);

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
        final Set<RequestMethod> previousMethods = this.handlerInfo.getHttpServer().getAllowedMethods();
        final EnumSet<RequestMethod> newMethods = EnumSet.noneOf(RequestMethod.class);
        for (final RequestMethod method : methods) {
            newMethods.add(method);
        }
        this.handlerInfo.getHttpServer().setAllowedMethods(newMethods);
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
            this.handlerInfo.getHttpServer().setAllowedMethods(methods);
        } else {
            this.handlerInfo.getHttpServer().setAllowedMethods(EnumSet.of(RequestMethod.GET));
        }
    }
}
