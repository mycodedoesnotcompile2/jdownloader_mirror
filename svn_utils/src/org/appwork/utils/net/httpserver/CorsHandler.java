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

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.requests.HttpRequest;

/**
 * Configuration and management class for CORS (Cross-Origin Resource Sharing) headers that should be added to HTTP responses.
 * 
 * <h2>Overview</h2>
 * This class provides a centralized way to configure and manage CORS-related HTTP response headers.
 * CORS headers control which origins are allowed to access resources on your server from web browsers.
 * 
 * <h2>Managed CORS Headers</h2>
 * This class manages the following CORS headers:
 * <ul>
 *   <li><b>Access-Control-Allow-Origin</b> - Specifies which origins are allowed to access the resource</li>
 *   <li><b>Access-Control-Expose-Headers</b> - Specifies which headers can be exposed to the client</li>
 *   <li><b>Access-Control-Allow-Methods</b> - Specifies which HTTP methods are allowed for cross-origin requests</li>
 *   <li><b>Access-Control-Allow-Headers</b> - Specifies which headers are allowed in cross-origin requests</li>
 *   <li><b>Access-Control-Max-Age</b> - Specifies how long the results of a preflight request can be cached</li>
 * </ul>
 * 
 * <h2>Default Configuration</h2>
 * <p>
 * By default, CORS is disabled (all fields are null). This means no CORS headers will be set unless explicitly configured.
 * For the HttpServer, this also means that requests containing an Origin header are rejected unless a CorsHandler is
 * explicitly configured. Requests without Origin headers are still allowed.
 * </p>
 * 
 * <h2>Usage Examples</h2>
 * 
 * <h3>Enable CORS for All Origins (Development Only)</h3>
 * <pre>{@code
 * CorsHandler corsHandler = new CorsHandler();
 * corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));
 * corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
 * corsHandler.setExposeHeaders("X-Session, X-ServerTime");
 * server.setCorsHandler(corsHandler);
 * }</pre>
 * 
 * <h3>Enable CORS for Specific Origins</h3>
 * <pre>{@code
 * CorsHandler corsHandler = new CorsHandler();
 * java.util.Set<String> origins = new java.util.HashSet<String>();
 * origins.add("https://example.com");
 * origins.add("https://another.example");
 * corsHandler.setAllowedOrigins(origins);
 * corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
 * server.setCorsHandler(corsHandler);
 * }</pre>
 * 
 * <h3>Disable CORS</h3>
 * <pre>{@code
 * server.setCorsHandler(null); // Disables CORS headers
 * }</pre>
 * 
 * <h2>Security Considerations</h2>
 * <ul>
 *   <li><b>Never use "*" in production</b> unless you explicitly need to allow all origins. This is a security risk.</li>
 *   <li><b>Use explicit origins</b> when possible to prevent unauthorized access.</li>
 *   <li><b>CORS headers are only relevant for browser requests</b>. Server-to-server communication doesn't use CORS.</li>
 *   <li><b>CORS does not replace authentication</b>. Always implement proper authentication and authorization.</li>
 * </ul>
 * 
 * @author AppWork
 */
public class CorsHandler {
    private boolean containsOrigin(final Set<String> origins, final String origin) {
        if (origins == null || origin == null) {
            return false;
        }
        for (final String allowedOrigin : origins) {
            if (StringUtils.equalsIgnoreCase(allowedOrigin, origin)) {
                return true;
            }
        }
        return false;
    }

    private Set<String> allowedOrigins;
    private String exposeHeaders;
    private Set<RequestMethod> allowMethods;
    private String allowHeaders;
    private boolean allowHeadersFromRequest = false;
    private Long maxAge;

    /**
     * Creates a new CorsHandler with all fields set to null (CORS disabled by default).
     */
    public CorsHandler() {
        // All fields are null by default (CORS disabled)
    }

    /**
     * Gets the allowed origins set.
     * 
     * @return The allowed origins set or null if not set
     */
    public Set<String> getAllowedOrigins() {
        return this.allowedOrigins != null ? new HashSet<String>(this.allowedOrigins) : null;
    }

    /**
     * Sets the allowed origins set.
     * 
     * <p>
     * This can be:
     * </p>
     * <ul>
     *   <li><b>Set containing "*"</b> - Allows all origins (use with caution in production)</li>
     *   <li><b>Set containing "https://example.com"</b> - Allows only requests from https://example.com</li>
     *   <li><b>null</b> - All Origin requests are forbidden</li>
     * </ul>
     * 
     * <p>
     * <b>Default behavior (null):</b> When set to null, any request with an Origin header will be rejected.
     * Requests without an Origin header are still allowed (direct navigation).
     *
     * <p>
     * <b>Note:</b> If no CorsHandler is configured on the HttpServer at all, the server will reject any request that
     * includes an Origin header, regardless of allowedOrigins.
     * </p>
     * </p>
     * 
     * @param allowedOrigins The allowed origins set, a set containing "*" for all origins, or null to forbid all Origin requests
     */
    public void setAllowedOrigins(final Set<String> allowedOrigins) {
        if (allowedOrigins != null && !allowedOrigins.isEmpty()) {
            final HashSet<String> normalized = new HashSet<String>();
            for (final String origin : allowedOrigins) {
                if (!StringUtils.isEmpty(origin)) {
                    normalized.add(origin.trim());
                }
            }
            this.allowedOrigins = normalized.isEmpty() ? null : normalized;
        } else {
            this.allowedOrigins = null;
        }
    }

    /**
     * Gets the list of headers that can be exposed to the client.
     * 
     * @return Comma-separated list of header names (e.g., "X-Session, X-ServerTime") or null
     */
    public String getExposeHeaders() {
        return this.exposeHeaders;
    }

    /**
     * Sets the list of headers that can be exposed to the client.
     * 
     * <p>
     * By default, only simple response headers are exposed to JavaScript. This setting allows you to expose
     * additional custom headers that the client needs to access.
     * </p>
     * 
     * @param exposeHeaders Comma-separated list of header names or null
     */
    public void setExposeHeaders(String exposeHeaders) {
        this.exposeHeaders = exposeHeaders;
    }

    /**
     * Gets the set of allowed HTTP methods for cross-origin requests.
     * 
     * @return Set of RequestMethod enum values or null
     */
    public Set<RequestMethod> getAllowMethods() {
        return this.allowMethods != null ? EnumSet.copyOf(this.allowMethods) : null;
    }

    /**
     * Sets the set of allowed HTTP methods for cross-origin requests.
     * 
     * <p>
     * This is typically used in preflight (OPTIONS) responses to indicate which methods are allowed.
     * The methods will be serialized as a comma-separated list in the Access-Control-Allow-Methods header.
     * </p>
     * 
     * @param allowMethods Set of RequestMethod enum values or null
     */
    public void setAllowMethods(Set<RequestMethod> allowMethods) {
        if (allowMethods != null && !allowMethods.isEmpty()) {
            this.allowMethods = EnumSet.copyOf(allowMethods);
        } else {
            this.allowMethods = null;
        }
    }

    /**
     * Gets the list of allowed headers in cross-origin requests.
     * 
     * @return Comma-separated list of header names or null
     */
    public String getAllowHeaders() {
        return this.allowHeaders;
    }

    /**
     * Sets the list of allowed headers in cross-origin requests.
     * 
     * <p>
     * This is typically used in preflight (OPTIONS) responses to indicate which headers are allowed.
     * </p>
     * 
     * <p>
     * <b>Note:</b> If {@link #setAllowHeadersFromRequest(boolean)} is set to true, the value from
     * Access-Control-Request-Headers in the request will be echoed back (like RemoteAPI.java), and this
     * static value will be ignored. If allowHeadersFromRequest is false, this static value will be used.
     * </p>
     * 
     * @param allowHeaders Comma-separated list of header names or null
     */
    public void setAllowHeaders(String allowHeaders) {
        this.allowHeaders = allowHeaders;
    }

    /**
     * Gets the max age for preflight request caching.
     * 
     * @return The max age in milliseconds or null
     */
    public Long getMaxAge() {
        return this.maxAge;
    }

    /**
     * Sets the max age for preflight request caching.
     * 
     * <p>
     * This specifies how long (in milliseconds) the results of a preflight request can be cached.
     * Common values are 30000 (30 seconds), 1800000 (30 minutes) or 86400000 (24 hours).
     * </p>
     * 
     * <p>
     * <b>Note:</b> The HTTP header Access-Control-Max-Age expects seconds, so this value will be
     * automatically converted to seconds when added to the response header.
     * </p>
     * 
     * @param maxAge The max age in milliseconds or null
     */
    public void setMaxAge(Long maxAge) {
        this.maxAge = maxAge;
    }

    /**
     * Sets whether Access-Control-Allow-Headers should be dynamically taken from the
     * Access-Control-Request-Headers header in the request (like RemoteAPI.java did).
     * 
     * <p>
     * <b>This is State-of-the-Art CORS behavior and the recommended setting.</b>
     * </p>
     * 
     * <p>
     * If true, the value from Access-Control-Request-Headers will be echoed back in
     * Access-Control-Allow-Headers. This is the standard CORS behavior for preflight requests
     * as defined in the CORS specification (W3C Fetch Standard).
     * </p>
     * 
     * <p>
     * <b>Security Note:</b> This is safe because:
     * </p>
     * <ul>
     *   <li>It only applies to preflight (OPTIONS) requests, not actual requests</li>
     *   <li>The browser validates that the actual request headers match what was requested</li>
     *   <li>The server can still validate and reject actual requests with unauthorized headers</li>
     *   <li>This is the standard practice used by major frameworks (Spring, Express, etc.)</li>
     * </ul>
     * 
     * <p>
     * If false (default), only the statically configured allowHeaders value will be used.
     * </p>
     * 
     * @param allowHeadersFromRequest true to enable dynamic allowHeaders from request (recommended), false otherwise
     */
    public void setAllowHeadersFromRequest(boolean allowHeadersFromRequest) {
        this.allowHeadersFromRequest = allowHeadersFromRequest;
    }

    /**
     * Gets whether Access-Control-Allow-Headers should be dynamically taken from the request.
     * 
     * @return true if allowHeaders should be taken from request, false otherwise
     */
    public boolean isAllowHeadersFromRequest() {
        return this.allowHeadersFromRequest;
    }

    /**
     * Validates whether the Origin header in the given request is allowed by this CORS configuration.
     *
     * <p>
     * This method does not add any headers. It only determines whether the origin should be accepted
     * based on the current configuration (allowedOrigins) or forbidden (null).
     * </p>
     *
     * @param request
     *            The HTTP request
     * @return true if the origin is allowed or no Origin header is present, false otherwise
     */
    public boolean isOriginAllowed(final HttpRequest request) {
        if (request == null) {
            return true;
        }
        final String origin = request.getRequestHeaders().getValue(HTTPConstants.HEADER_REQUEST_ORIGIN);
        if (origin == null || origin.isEmpty()) {
            // No Origin header present - allow (direct browser navigation)
            return true;
        }
        if (this.allowedOrigins == null) {
            return false;
        }
        if (this.allowedOrigins.contains("*")) {
            return true;
        }
        return containsOrigin(this.allowedOrigins, origin);
    }

    /**
     * Adds CORS headers to the response if CORS is enabled and the request is a cross-origin request.
     * 
     * <p>
     * This method checks if the request has an Origin header (indicating a cross-origin request).
     * If CORS is enabled and the origin is allowed, it adds the appropriate CORS headers to the response.
     * </p>
     * 
     * <p>
     * <b>Note:</b> Headers are only added if they are not already present, allowing handlers to override defaults.
     * </p>
     * 
     * @param request The HTTP request
     * @param responseHeaders The response headers collection
     */
    public void addCorsHeaders(HttpRequest request, HeaderCollection responseHeaders) {
        if (responseHeaders == null || request == null) {
            return;
        }

        // Check if this is a cross-origin request (has Origin header)
        String origin = request.getRequestHeaders().getValue(HTTPConstants.HEADER_REQUEST_ORIGIN);
        if (origin == null || origin.isEmpty()) {
            // Not a cross-origin request, no CORS headers needed
            return;
        }

        // Determine allowed origin
        String allowedOrigin = null;
        if (this.allowedOrigins != null) {
            // If allowedOrigins contains "*", allow all; otherwise allow only if origin matches
            if (this.allowedOrigins.contains("*")) {
                allowedOrigin = "*";
            } else if (containsOrigin(this.allowedOrigins, origin)) {
                allowedOrigin = origin;
            } else {
                allowedOrigin = null;
            }
        } else {
            // No allowed origins configured -> forbid all Origin requests
            allowedOrigin = null;
        }

        // If no origin is allowed, don't add CORS headers
        if (allowedOrigin == null) {
            return;
        }

        // Add Access-Control-Allow-Origin
        if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN) == null) {
            responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN, allowedOrigin));
        }

        // Add Access-Control-Expose-Headers
        if (this.exposeHeaders != null && !this.exposeHeaders.isEmpty()) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS) == null) {
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS, this.exposeHeaders));
            }
        }

        // Add Access-Control-Allow-Methods (typically for OPTIONS/preflight requests)
        if (this.allowMethods != null && !this.allowMethods.isEmpty()) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS) == null) {
                // Convert Set<RequestMethod> to comma-separated string
                StringBuilder methodsBuilder = new StringBuilder();
                boolean first = true;
                for (RequestMethod method : this.allowMethods) {
                    if (!first) {
                        methodsBuilder.append(", ");
                    }
                    // Special case: MSEARCH enum represents "M-SEARCH" HTTP method
                    String methodName = "MSEARCH".equals(method.name()) ? "M-SEARCH" : method.name();
                    methodsBuilder.append(methodName);
                    first = false;
                }
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS, methodsBuilder.toString()));
            }
        }

        // Add Access-Control-Allow-Headers (typically for OPTIONS/preflight requests)
        // Support dynamic allowHeaders from request (like RemoteAPI.java)
        if (this.allowHeadersFromRequest) {
            String requestedHeaders = request.getRequestHeaders().getValue(HTTPConstants.HEADER_REQUEST_CONTROL_HEADERS);
            if (requestedHeaders != null && !requestedHeaders.isEmpty()) {
                if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS) == null) {
                    responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS, requestedHeaders));
                }
            }
        } else if (this.allowHeaders != null && !this.allowHeaders.isEmpty()) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS) == null) {
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS, this.allowHeaders));
            }
        }

        // Add Access-Control-Max-Age (typically for OPTIONS/preflight requests)
        // Note: HTTP header expects seconds, but we store milliseconds internally
        if (this.maxAge != null) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE) == null) {
                // Convert milliseconds to seconds for the HTTP header
                long maxAgeSeconds = this.maxAge / 1000;
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE, String.valueOf(maxAgeSeconds)));
            }
        }
    }
}

