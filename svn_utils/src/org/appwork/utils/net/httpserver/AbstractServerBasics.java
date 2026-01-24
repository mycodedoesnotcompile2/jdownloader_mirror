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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver;

import java.io.IOException;
import java.net.SocketException;
import java.nio.channels.ClosedChannelException;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractPostRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.HttpServerInterface;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Abstract base class for HTTP server implementations that provides common security, CORS, limits, and configuration functionality.
 *
 * This class implements HttpServerInterface and provides default implementations for all common configuration methods. Classes that need
 * HTTP server functionality should extend this class rather than implementing HttpServerInterface directly.
 *
 * @author AppWork GmbH
 */
public abstract class AbstractServerBasics implements HttpServerInterface {

    // Security and CORS configuration
    protected ResponseSecurityHeaders responseSecurityHeaders;
    protected CorsHandler             corsHandler;

    // HttpServerInterface configuration
    protected Set<RequestMethod>      allowedMethods;
    protected HeaderValidationRules   headerValidationRules;
    protected RequestSizeLimits       requestSizeLimits;
    protected ConnectionTimeouts      connectionTimeouts;

    // Server header configuration
    protected String                  responseServerHeaderValue;

    /**
     * Constructor for AbstractServerBasics. Initializes default values for security and configuration settings.
     *
     * @param name
     */
    protected AbstractServerBasics(String name) {
        setResponseServerHeaderValue(name);

        // Set default size limits
        this.requestSizeLimits = new RequestSizeLimits();

        // Set default header validation rules
        this.headerValidationRules = new HeaderValidationRules();
        // Set default allowed methods (only GET allowed by default)
        this.allowedMethods = EnumSet.of(RequestMethod.GET);
        // Set default security headers config
        this.responseSecurityHeaders = new ResponseSecurityHeaders();
        // Set default connection timeouts config
        this.connectionTimeouts = new ConnectionTimeouts();
        this.corsHandler = new CorsHandler();
        //
        System.setProperty("TESTTTT", "hj213");
    }

    public void setAllowedMethods(RequestMethod... methods) {
        this.setAllowedMethods(new HashSet<RequestMethod>(Arrays.asList(methods)));

    }
    // HttpServerInterface implementation

    @Override
    public ResponseSecurityHeaders getResponseSecurityHeaders() {
        return this.responseSecurityHeaders;
    }

    /**
     * Sets the response security headers configuration.
     *
     * @param responseSecurityHeaders
     *            The response security headers instance, or null to disable
     */
    public void setResponseSecurityHeaders(final ResponseSecurityHeaders responseSecurityHeaders) {
        this.responseSecurityHeaders = responseSecurityHeaders;
    }

    @Override
    public Set<RequestMethod> getAllowedMethods() {
        return this.allowedMethods != null ? EnumSet.copyOf(this.allowedMethods) : null;
    }

    /**
     * Sets the allowed HTTP methods. Set to null to disable method validation.
     *
     * @param allowedMethods
     *            Set of allowed HTTP methods. If null or empty, method validation is disabled.
     */
    public void setAllowedMethods(final Set<RequestMethod> allowedMethods) {
        if (allowedMethods != null && !allowedMethods.isEmpty()) {
            this.allowedMethods = EnumSet.copyOf(allowedMethods);
        } else {
            this.allowedMethods = null;
        }
    }

    @Override
    public HeaderValidationRules getHeaderValidationRules() {
        return this.headerValidationRules;
    }

    /**
     * Sets the header validation rules. Set to null to disable header validation.
     *
     * @param headerValidationRules
     *            The header validation rules instance, or null to disable
     */
    public void setHeaderValidationRules(final HeaderValidationRules headerValidationRules) {
        this.headerValidationRules = headerValidationRules;
    }

    @Override
    public RequestSizeLimits getRequestSizeLimits() {
        return this.requestSizeLimits;
    }

    /**
     * Sets the request size limits. Set to null to disable size limits.
     *
     * @param requestSizeLimits
     *            The request size limits instance, or null to disable
     */
    public void setRequestSizeLimits(final RequestSizeLimits requestSizeLimits) {
        this.requestSizeLimits = requestSizeLimits;
    }

    @Override
    public ConnectionTimeouts getConnectionTimeouts() {
        return this.connectionTimeouts;
    }

    /**
     * Sets the connection timeouts configuration. Set to null to disable timeouts (not recommended).
     *
     * @param connectionTimeouts
     *            The connection timeouts instance, or null to disable
     */
    public void setConnectionTimeouts(final ConnectionTimeouts connectionTimeouts) {
        this.connectionTimeouts = connectionTimeouts;
    }

    @Override
    public CorsHandler getCorsHandler() {
        return this.corsHandler;
    }

    /**
     * Sets the CORS handler.
     *
     * @param corsHandler
     *            The CORS handler instance, or null to disable CORS
     */
    public void setCorsHandler(final CorsHandler corsHandler) {
        if (corsHandler != null) {
            // Validate CORS configuration, including conflicts with security headers if configured
            corsHandler.validate();
        }
        this.corsHandler = corsHandler;
    }

    @Override
    public String getResponseServerHeader() {
        return this.responseServerHeaderValue;
    }

    /**
     * Sets the Server response header value. Set to null to disable the Server header.
     *
     * @param responseServerHeader
     *            The Server header value, or null to disable
     */
    public void setResponseServerHeaderValue(final String responseServerHeader) {
        this.responseServerHeaderValue = responseServerHeader;
    }

    // Abstract methods that must be implemented by subclasses

    /**
     * Returns whether chunked encoded responses are allowed for the given request/response.
     *
     * @param httpRequest
     *            The HTTP request
     * @param httpResponse
     *            The HTTP response
     * @return true if chunked encoding is allowed, false otherwise
     */
    @Override
    public abstract boolean isChunkedEncodedResponseAllowed(HttpRequest httpRequest, HttpResponse httpResponse);

    /**
     * Returns the list of request handlers.
     *
     * @return The list of request handlers
     */
    @Override
    public abstract List<HttpRequestHandler> getHandler();

    /**
     * Validates an HTTP request according to the server's security configuration. This includes header validation, method validation, and
     * origin/CORS validation.
     *
     * <p>
     * This method should be called before processing any request to ensure security policies are enforced. It throws exceptions if
     * validation fails.
     * </p>
     *
     * @param requestMethod
     *
     * @param headerCollection
     *            The request to validate. Must implement CorsRequestInterface and provide getRequestMethod(), getRequestedURL() or
     *            getRequestURI(), and getRemoteAddress()
     * @throws ForbiddenHeaderException
     *             if header validation fails
     * @throws HttpMethodNotAllowedException
     *             if the HTTP method is not allowed
     * @throws ForbiddenOriginException
     *             if the origin is not allowed (CORS validation fails)
     */
    public void validateRequest(HttpRequest request) throws IOException, ForbiddenHeaderException, ForbiddenOriginException {

        final HeaderCollection requestHeaders = request.getRequestHeaders();

        // Header validation
        final HeaderValidationRules headerRules = this.getHeaderValidationRules();
        if (headerRules != null && headerRules.isEnabled()) {
            if (!headerRules.isRequestAllowed(requestHeaders)) {
                final String errorMessage = headerRules.getValidationError(requestHeaders);

                LogV3.warning("Security: Header validation failed | Error: " + errorMessage);
                throw new ForbiddenHeaderException(errorMessage);
            }
        }

        // Method validation
        final Set<RequestMethod> allowedMethods = this.getAllowedMethods();
        RequestMethod requestMethod = request.getRequestMethod();
        if (allowedMethods != null) {

            if (requestMethod == RequestMethod.UNKNOWN || !allowedMethods.contains(requestMethod)) {
                throw new HttpMethodNotAllowedException("Illegal Method");
            }
        }

        // Origin validation (CORS)
        final HTTPHeader originHeader = requestHeaders.get(HTTPConstants.HEADER_REQUEST_ORIGIN);
        if (originHeader != null) {
            final String origin = originHeader.getValue();
            if (StringUtils.isEmpty(origin)) {
                throw new ForbiddenOriginException("Empty Origin header rejected");
            }
            final CorsHandler corsHandler = this.getCorsHandler();
            // Only validate if CORS handler is configured (null means CORS validation is disabled)
            if (corsHandler != null) {
                if (!corsHandler.isOriginAllowed(requestHeaders)) {
                    throw new ForbiddenOriginException("Cross-Origin request rejected");
                }
            }
            // If corsHandler is null, skip validation (CORS disabled)
        }
        RequestSizeLimits limits = getRequestSizeLimits();
        // Early Content-Length check: Reject requests with Content-Length exceeding POST body limit BEFORE reading any body data
        if (request instanceof AbstractPostRequest && limits != null && limits.isPostBodySizeLimitEnabled()) {
            final HTTPHeader contentLengthHeader = requestHeaders.get(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH);
            if (contentLengthHeader != null) {
                try {
                    final long contentLength = Long.parseLong(contentLengthHeader.getValue());
                    final long maxPostBodySize = limits.getMaxPostBodySize();
                    if (contentLength > maxPostBodySize) {
                        throw new RequestSizeLimitExceededException("Request size limit exceeded. Content-Length: " + contentLength + " bytes exceeds maximum allowed size: " + maxPostBodySize + " bytes");
                    }
                } catch (NumberFormatException e) {
                    // Invalid Content-Length header, let it fail during body reading
                }
            }
        }

    }

    // /**
    // * Helper method to extract the request method from a request object. Works with both HttpRequest and HTTPServerRequest.
    // *
    // * @param request
    // * The request object
    // * @return The request method, or UNKNOWN if not available
    // */
    // protected RequestMethod getRequestMethod(final CorsRequestInterface request) {
    // if (request instanceof HttpRequest) {
    // return ((HttpRequest) request).getRequestMethod();
    // }
    // // Try to get method via reflection for HTTPServerRequest (from FastCGIInterface package)
    // try {
    // final java.lang.reflect.Method method = request.getClass().getMethod("getRequestMethod");
    // final Object result = method.invoke(request);
    // if (result instanceof RequestMethod) {
    // return (RequestMethod) result;
    // }
    // } catch (final Throwable e) {
    // // Ignore - method not available
    // }
    // return RequestMethod.UNKNOWN;
    // }
    //
    // /**
    // * Helper method to extract the request URL from a request object. Works with both HttpRequest and HTTPServerRequest.
    // *
    // * @param request
    // * The request object
    // * @return The request URL/URI, or "unknown" if not available
    // */
    // protected String getRequestUrlString(final CorsRequestInterface request) {
    // if (request instanceof HttpRequest) {
    // return ((HttpRequest) request).getRequestedURL();
    // }
    // // Try to get URI via reflection for HTTPServerRequest (from FastCGIInterface package)
    // try {
    // final java.lang.reflect.Method method = request.getClass().getMethod("getRequestURI");
    // final Object result = method.invoke(request);
    // if (result instanceof String) {
    // return (String) result;
    // }
    // } catch (final Throwable e) {
    // // Ignore - method not available
    // }
    // return "unknown";
    // }

    // /**
    // * Helper method to extract the remote address from a request object. Works with both HttpRequest and HTTPServerRequest.
    // *
    // * @param request
    // * The request object
    // * @return The remote address as a comma-separated string, or "unknown" if not available
    // */
    // protected String getRemoteAddressString(final CorsRequestInterface request) {
    // if (request instanceof HttpRequest) {
    // final List<String> remoteAddresses = ((HttpRequest) request).getRemoteAddress();
    // if (remoteAddresses != null && !remoteAddresses.isEmpty()) {
    // return StringUtils.join(remoteAddresses, ", ");
    // }
    // } else {
    // // Try to get remote address via reflection for HTTPServerRequest (from FastCGIInterface package)
    // try {
    // final java.lang.reflect.Method method = request.getClass().getMethod("getRemoteAddress");
    // final Object result = method.invoke(request);
    // if (result instanceof String) {
    // final String remoteAddress = (String) result;
    // if (!StringUtils.isEmpty(remoteAddress)) {
    // return remoteAddress;
    // }
    // }
    // } catch (final Throwable e) {
    // // Ignore - method not available
    // }
    // }
    // return "unknown";
    // }

    /**
     * Result class for onException method that contains both the response and close connection flag.
     */
    public static class ExceptionHandlerResult {
        public final HttpResponse response;
        public final boolean      closeConnection;

        public ExceptionHandlerResult(final HttpResponse response, final boolean closeConnection) {
            this.response = response;
            this.closeConnection = closeConnection;
        }
    }

    /**
     * Handles exceptions that occur during HTTP request processing. This method is called by HttpConnection when an exception is caught
     * during request handling.
     *
     * <p>
     * The method handles various exception types:
     * <ul>
     * <li>SocketException/ClosedChannelException: Socket already closed (timeout or security limit) - returns true to close connection</li>
     * <li>HttpMethodNotAllowedException: HTTP method not allowed - returns 405 Method Not Allowed</li>
     * <li>RequestSizeLimitExceededException: Request size limit exceeded - returns 413 Request Entity Too Large</li>
     * <li>ForbiddenOriginException: Forbidden origin detected - returns 403 Forbidden</li>
     * <li>ForbiddenHeaderException: Forbidden header detected - returns 400 Bad Request</li>
     * <li>HttpConnectionExceptionHandler: Custom exception handler - delegates to exception's handle method</li>
     * <li>Other exceptions: Returns 500 Internal Server Error</li>
     * </ul>
     * </p>
     *
     * @param e
     *            The exception that occurred
     * @param request
     *            The HTTP request (may be null if exception occurred before request was built)
     * @param response
     *            The HTTP response (may be null, will be created if needed)
     * @param connection
     *            The HTTP connection interface (used to create new HttpResponse if needed)
     * @return ExceptionHandlerResult containing the response and whether to close the connection
     * @throws IOException
     *             if an I/O error occurs while handling the exception
     */
    public boolean onException(final Throwable e, final HttpRequest request, HttpResponse response) throws IOException {

        if (Exceptions.containsInstanceOf(e, SocketException.class, ClosedChannelException.class)) {
            // socket already closed (likely due to timeout or security limit)
            // Don't log as error - this is expected when timeouts trigger or security limits are exceeded
            return true;

        } else if (e instanceof HttpMethodNotAllowedException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            final String method = request != null && request.getRequestMethod() != null ? request.getRequestMethod().name() : "unknown";
            LogV3.warning("Security: HTTP method not allowed | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Method: " + method + " | Message: " + e.getMessage());
            response.setResponseCode(ResponseCode.METHOD_NOT_ALLOWED);
            // Apply default headers (security, CORS, server) before setting content

            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = e.getMessage() != null ? e.getMessage() : "HTTP method not allowed";
                final byte[] bytes = message.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                response.getOutputStream(true).write(bytes);
                response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;

        } else if (request instanceof OptionsRequest) {
            response.setResponseCode(HTTPConstants.ResponseCode.ERROR_FORBIDDEN);
            // Apply default headers (security, CORS, server) before setting content

            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, "0"));
            response.getOutputStream(true).flush();
            return true;
        } else if (e instanceof RequestSizeLimitExceededException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Request size limit exceeded | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            response.setResponseCode(ResponseCode.REQUEST_ENTITY_TOO_LARGE);
            // Apply default headers (security, CORS, server) before setting content

            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Request size limit exceeded!";
                final byte[] bytes = message.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                response.getOutputStream(true).write(bytes);
                response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;

        } else if (e instanceof ForbiddenOriginException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Forbidden origin detected | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            response.setResponseCode(ResponseCode.ERROR_FORBIDDEN);
            // Apply default headers (security, CORS, server) before setting content

            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Forbidden Origin!";
                final byte[] bytes = message.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                response.getOutputStream(true).write(bytes);
                response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;
        } else if (e instanceof ForbiddenHeaderException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Forbidden header detected | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            response.setResponseCode(ResponseCode.ERROR_BAD_REQUEST);
            // Apply default headers (security, CORS, server) before setting content

            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Forbidden Headers!";
                final byte[] bytes = message.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                response.getOutputStream(true).write(bytes);
                response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;
        } else if (e instanceof HttpConnectionExceptionHandler) {
            return ((HttpConnectionExceptionHandler) e).handle(request, response);

        } else if (request != null) {
            response.setResponseCode(ResponseCode.SERVERERROR_INTERNAL);
            LogV3.log(e);
            // Apply default headers (security, CORS, server) before setting content

            // do not return stacktraces!
            final byte[] bytes = "Unexpected Exception".getBytes("UTF-8");
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text; charset=UTF-8"));
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
            response.getOutputStream(true).write(bytes);
            response.getOutputStream(true).flush();
            return true;
        } else {
            // too early - no request read so far
            LogV3.log(e);
            return true;
        }
    }

}
