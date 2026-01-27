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

import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Configuration and management class for CORS (Cross-Origin Resource Sharing) headers that should be added to HTTP responses.
 *
 * <h2>Overview</h2> This class provides a centralized way to configure and manage CORS-related HTTP response headers. CORS headers control
 * which origins are allowed to access resources on your server from web browsers.
 *
 * <h2>Managed CORS Headers</h2> This class manages the following CORS headers:
 * <ul>
 * <li><b>Access-Control-Allow-Origin</b> - Specifies which origins are allowed to access the resource</li>
 * <li><b>Access-Control-Allow-Credentials</b> - Specifies whether credentials (cookies, authorization headers) can be included in
 * cross-origin requests</li>
 * <li><b>Access-Control-Allow-Private-Network</b> - Specifies whether private network access is allowed (for requests from public to
 * private networks)</li>
 * <li><b>Access-Control-Expose-Headers</b> - Specifies which headers can be exposed to the client</li>
 * <li><b>Access-Control-Allow-Methods</b> - Specifies which HTTP methods are allowed for cross-origin requests</li>
 * <li><b>Access-Control-Allow-Headers</b> - Specifies which headers are allowed in cross-origin requests</li>
 * <li><b>Access-Control-Max-Age</b> - Specifies how long the results of a preflight request can be cached</li>
 * </ul>
 *
 * <h2>Default Configuration</h2>
 * <p>
 * By default, CORS is disabled (all fields are null). This means no CORS headers will be set unless explicitly configured. For the
 * HttpServer, this also means that requests containing an Origin header are rejected unless a CorsHandler is explicitly configured.
 * Requests without Origin headers are still allowed.
 * </p>
 *
 * <p>
 * <b>Security defaults (restrictive):</b>
 * </p>
 * <ul>
 * <li><b>Access-Control-Allow-Credentials:</b> Disabled by default (false). Must be explicitly enabled if credentials are needed.</li>
 * <li><b>Access-Control-Allow-Private-Network:</b> Disabled by default (false). Must be explicitly enabled if private network access is
 * needed.</li>
 * </ul>
 *
 * <h2>Usage Examples</h2>
 *
 * <h3>Enable CORS for All Origins (Development Only)</h3>
 *
 * <pre>{@code
 * CorsHandler corsHandler = new CorsHandler();
 * corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));
 * corsHandler.setAllowMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS));
 * corsHandler.setExposeHeaders("X-Session, X-ServerTime");
 * server.setCorsHandler(corsHandler);
 * }</pre>
 *
 * <h3>Enable CORS for Specific Origins</h3>
 *
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
 *
 * <pre>{@code
 * server.setCorsHandler(null); // Disables CORS headers
 * }</pre>
 *
 * <h3>Pattern-based Rules for Credentials and Private Network Access</h3>
 *
 * <pre>{@code
 * CorsHandler corsHandler = new CorsHandler();
 * corsHandler.setAllowedOrigins(java.util.Collections.singleton("*"));
 *
 * // Deny credentials for all origins by default
 * corsHandler.addCredentialsRule(".*", false);
 * // But allow credentials for specific trusted origin (higher priority - checked last)
 * corsHandler.addCredentialsRule("https://example\\.com", true);
 *
 * // Same for Private Network Access
 * corsHandler.addPrivateNetworkRequestRule(".*", false);
 * corsHandler.addPrivateNetworkRequestRule("https://trusted\\.example\\.com", true);
 *
 * server.setCorsHandler(corsHandler);
 * }</pre>
 *
 * <p>
 * <b>Rule Priority:</b> Rules are checked in reverse order (last entry has highest priority). If the first rule is ".*":false and the
 * second is "https://example\\.com":true, all origins are denied except example.com. Without any rules, the default is deny (false).
 * </p>
 *
 * <h2>Security Considerations</h2>
 * <ul>
 * <li><b>Never use "*" in production</b> unless you explicitly need to allow all origins. This is a security risk.</li>
 * <li><b>Use explicit origins</b> when possible to prevent unauthorized access.</li>
 * <li><b>CORS headers are only relevant for browser requests</b>. Server-to-server communication doesn't use CORS.</li>
 * <li><b>CORS does not replace authentication</b>. Always implement proper authentication and authorization.</li>
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

    private Set<String>                     allowedOrigins;
    private LinkedHashMap<Pattern, Boolean> credentialsRules           = new LinkedHashMap<Pattern, Boolean>();
    private LinkedHashMap<Pattern, Boolean> privateNetworkRequestRules = new LinkedHashMap<Pattern, Boolean>();
    private boolean                         enableSecurityValidation   = true;
    private String                          exposeHeaders;
    private Set<RequestMethod>              allowMethods;
    private String                          allowHeaders;
    private boolean                         allowHeadersFromRequest    = false;
    private Long                            maxAge;

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
     * <li><b>Set containing "*"</b> - Allows all origins (use with caution in production)</li>
     * <li><b>Set containing "https://example.com"</b> - Allows only requests from https://example.com</li>
     * <li><b>null</b> - All Origin requests are forbidden</li>
     * </ul>
     *
     * <p>
     * <b>Default behavior (null):</b> When set to null, any request with an Origin header will be rejected. Requests without an Origin
     * header are still allowed (direct navigation).
     *
     * <p>
     * <b>Note:</b> If no CorsHandler is configured on the HttpServer at all, the server will reject any request that includes an Origin
     * header, regardless of allowedOrigins.
     * </p>
     * </p>
     *
     * <p>
     * <b>Security Validation:</b> If allowCredentials is true, "*" cannot be used as an allowed origin. This will throw an
     * IllegalArgumentException.
     * </p>
     *
     * @param allowedOrigins
     *            The allowed origins set, a set containing "*" for all origins, or null to forbid all Origin requests
     * @throws IllegalArgumentException
     *             if allowCredentials is true and allowedOrigins contains "*"
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
        this.validate();
    }

    public final void setAllowMethods(RequestMethod... methods) {
        setAllowMethods(new HashSet<RequestMethod>(Arrays.asList(methods)));

    }

    /**
     * Gets whether credentials (cookies, authorization headers) are allowed in cross-origin requests for a specific origin.
     *
     * <p>
     * This method checks pattern-based rules first (if configured), then falls back to the global allowCredentials setting.
     * </p>
     *
     * <p>
     * <b>Default: false (restrictive)</b> - Credentials are disabled by default. You must explicitly enable this if your service needs to
     * accept credentials in cross-origin requests.
     * </p>
     *
     * <p>
     * <b>Pattern-based rules:</b> If credentialsRules are configured, they are checked in reverse order (last entry has highest priority).
     * If no rule matches, the default is false (deny).
     * </p>
     *
     * @param origin
     *            The origin to check (e.g., "https://example.com"). If null, returns the global setting.
     * @return true if credentials are allowed for the given origin, false otherwise (default: false)
     */
    public boolean isAllowCredentials(String origin) {
        if (origin == null || origin.isEmpty()) {
            // For null/empty origin, default deny
            return false;
        }
        // Check pattern-based rules (in reverse order for priority)
        if (!this.credentialsRules.isEmpty()) {
            List<Map.Entry<Pattern, Boolean>> rulesList = new java.util.ArrayList<Map.Entry<Pattern, Boolean>>(this.credentialsRules.entrySet());
            // Iterate in reverse order (last entry has highest priority)
            for (int i = rulesList.size() - 1; i >= 0; i--) {
                Map.Entry<Pattern, Boolean> rule = rulesList.get(i);
                if (rule.getKey().matcher(origin).matches()) {
                    return rule.getValue();
                }
            }
        }
        // No rule matched - default deny
        return false;
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
     * By default, only simple response headers are exposed to JavaScript. This setting allows you to expose additional custom headers that
     * the client needs to access.
     * </p>
     *
     * @param exposeHeaders
     *            Comma-separated list of header names or null
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
     * This is typically used in preflight (OPTIONS) responses to indicate which methods are allowed. The methods will be serialized as a
     * comma-separated list in the Access-Control-Allow-Methods header.
     * </p>
     *
     * @param allowMethods
     *            Set of RequestMethod enum values or null
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
     * <b>Note:</b> If {@link #setAllowHeadersFromRequest(boolean)} is set to true, the value from Access-Control-Request-Headers in the
     * request will be echoed back (like RemoteAPI.java), and this static value will be ignored. If allowHeadersFromRequest is false, this
     * static value will be used.
     * </p>
     *
     * @param allowHeaders
     *            Comma-separated list of header names or null
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
     * This specifies how long (in milliseconds) the results of a preflight request can be cached. Common values are 30000 (30 seconds),
     * 1800000 (30 minutes) or 86400000 (24 hours).
     * </p>
     *
     * <p>
     * <b>Note:</b> The HTTP header Access-Control-Max-Age expects seconds, so this value will be automatically converted to seconds when
     * added to the response header.
     * </p>
     *
     * @param maxAge
     *            The max age in milliseconds or null
     */
    public void setMaxAge(Long maxAge) {
        this.maxAge = maxAge;
    }

    /**
     * Sets whether Access-Control-Allow-Headers should be dynamically taken from the Access-Control-Request-Headers header in the request
     * (like RemoteAPI.java did).
     *
     * <p>
     * <b>This is State-of-the-Art CORS behavior and the recommended setting.</b>
     * </p>
     *
     * <p>
     * If true, the value from Access-Control-Request-Headers will be echoed back in Access-Control-Allow-Headers. This is the standard CORS
     * behavior for preflight requests as defined in the CORS specification (W3C Fetch Standard).
     * </p>
     *
     * <p>
     * <b>Security Note:</b> This is safe because:
     * </p>
     * <ul>
     * <li>It only applies to preflight (OPTIONS) requests, not actual requests</li>
     * <li>The browser validates that the actual request headers match what was requested</li>
     * <li>The server can still validate and reject actual requests with unauthorized headers</li>
     * <li>This is the standard practice used by major frameworks (Spring, Express, etc.)</li>
     * </ul>
     *
     * <p>
     * If false (default), only the statically configured allowHeaders value will be used.
     * </p>
     *
     * @param allowHeadersFromRequest
     *            true to enable dynamic allowHeaders from request (recommended), false otherwise
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
     * Gets whether Access-Control-Allow-Private-Network should be dynamically taken from the request for a specific origin.
     *
     * <p>
     * This method checks pattern-based rules first (if configured), then falls back to the global allowPrivateNetworkFromRequest setting.
     * </p>
     *
     * <p>
     * <b>Default: false (restrictive)</b> - Private Network Access is disabled by default. You must explicitly enable it if your service
     * needs to accept requests from public websites to private network endpoints.
     * </p>
     *
     * <p>
     * <b>Pattern-based rules:</b> If privateNetworkRequestRules are configured, they are checked in reverse order (last entry has highest
     * priority). If no rule matches, the default is false (deny).
     * </p>
     *
     * @param origin
     *            The origin to check (e.g., "https://example.com"). If null, returns the global setting.
     * @return true if Private Network Access should be allowed for the given origin, false otherwise (default: false)
     */
    public boolean isAllowPrivateNetworkFromRequest(String origin) {
        if (origin == null || origin.isEmpty()) {
            // For null/empty origin, default deny
            return false;
        }
        // Check pattern-based rules (in reverse order for priority)
        if (!this.privateNetworkRequestRules.isEmpty()) {
            List<Map.Entry<Pattern, Boolean>> rulesList = new java.util.ArrayList<Map.Entry<Pattern, Boolean>>(this.privateNetworkRequestRules.entrySet());
            // Iterate in reverse order (last entry has highest priority)
            for (int i = rulesList.size() - 1; i >= 0; i--) {
                Map.Entry<Pattern, Boolean> rule = rulesList.get(i);
                if (rule.getKey().matcher(origin).matches()) {
                    return rule.getValue();
                }
            }
        }
        // No rule matched - default deny
        return false;
    }

    /**
     * Adds a pattern-based rule for credentials using a Pattern instance. Rules are checked in reverse order (last entry has highest
     * priority).
     *
     * <p>
     * Example: If you add Pattern.compile(".*"):false first, then Pattern.compile("https://example\\.com"):true, all origins are denied
     * except example.com.
     * </p>
     *
     * @param pattern
     *            The Pattern instance to match against the origin
     * @param allow
     *            true to allow credentials for matching origins, false to deny
     */
    public void addCredentialsRule(Pattern pattern, boolean allow) {
        this.credentialsRules.put(pattern, allow);
        this.validate();
    }

    /**
     * Removes all credentials rules.
     */
    public void clearCredentialsRules() {
        this.credentialsRules.clear();
        this.validate();
    }

    /**
     * Gets a copy of the credentials rules map.
     *
     * @return A copy of the credentials rules map
     */
    public LinkedHashMap<Pattern, Boolean> getCredentialsRules() {
        return new LinkedHashMap<Pattern, Boolean>(this.credentialsRules);
    }

    /**
     * Adds a pattern-based rule for private network access using a Pattern instance. Rules are checked in reverse order (last entry has
     * highest priority).
     *
     * <p>
     * Example: If you add Pattern.compile(".*"):false first, then Pattern.compile("https://example\\.com"):true, all origins are denied
     * except example.com.
     * </p>
     *
     * @param pattern
     *            The Pattern instance to match against the origin
     * @param allow
     *            true to allow private network access for matching origins, false to deny
     */
    public void addPrivateNetworkRequestRule(Pattern pattern, boolean allow) {
        this.privateNetworkRequestRules.put(pattern, allow);
        this.validate();
    }

    /**
     * Removes all private network request rules.
     */
    public void clearPrivateNetworkRequestRules() {
        this.privateNetworkRequestRules.clear();
        this.validate();
    }

    /**
     * Gets a copy of the private network request rules map.
     *
     * @return A copy of the private network request rules map
     */
    public LinkedHashMap<Pattern, Boolean> getPrivateNetworkRequestRules() {
        return new LinkedHashMap<Pattern, Boolean>(this.privateNetworkRequestRules);
    }

    /**
     * Gets whether security validation is enabled.
     *
     * @return true if security validation is enabled (default: true), false otherwise
     */
    public boolean isEnableSecurityValidation() {
        return this.enableSecurityValidation;
    }

    /**
     * Sets whether security validation is enabled.
     *
     * <p>
     * When enabled (default: true), the validate() method will check for dangerous or contradictory configurations and throw
     * IllegalStateException if found. When disabled (false), only critical errors that would cause direct runtime failures are checked.
     * </p>
     *
     * @param enableSecurityValidation
     *            true to enable security validation (default), false to disable (only critical errors checked)
     */
    public void setEnableSecurityValidation(boolean enableSecurityValidation) {
        this.enableSecurityValidation = enableSecurityValidation;
    }

    public void validate() {
        if (!this.enableSecurityValidation) {
            // Only check for critical errors that would cause direct runtime failures
            // Currently no critical-only checks implemented
            return;
        }

        // Check if "*" origin is combined with credentials rules that allow credentials
        // CORS specification: When Access-Control-Allow-Origin is "*", Access-Control-Allow-Credentials cannot be "true"
        if (this.allowedOrigins != null && this.allowedOrigins.contains("*")) {
            // Check if any credentials rule allows credentials
            for (Map.Entry<Pattern, Boolean> rule : this.credentialsRules.entrySet()) {
                if (rule.getValue()) {
                    // This rule allows credentials - this is incompatible with "*" origin
                    // Note: Even if the rule only matches specific origins, the fact that "*" is in allowedOrigins
                    // means the server could respond with "*" for some requests, which violates CORS spec when credentials are involved
                    throw new IllegalStateException("Cannot use Access-Control-Allow-Origin \"*\" when credentials are allowed via pattern-based rules. This violates CORS specification and is a security risk. Use specific origins instead of \"*\" or remove/deny credentials rules that allow credentials.");
                }
            }
        }

    }

    /**
     * Validates whether the Origin header in the given request is allowed by this CORS configuration.
     *
     * <p>
     * This method does not add any headers. It only determines whether the origin should be accepted based on the current configuration
     * (allowedOrigins) or forbidden (null).
     * </p>
     *
     * @param request
     *            The HTTP request (must implement CorsRequestInterface)
     * @return true if the origin is allowed or no Origin header is present, false otherwise
     */
    public boolean isOriginAllowed(final HeaderCollection requestHeaders) {
        if (requestHeaders == null) {
            return true;
        }
        final String origin = requestHeaders.getValue(HTTPConstants.HEADER_REQUEST_ORIGIN);
        if (origin == null || origin.isEmpty()) {
            // No Origin header present - allow (direct browser navigation)
            return true;
        }
        Set<String> ao = getAllowedOrigins();
        if (ao == null) {
            return false;
        }
        if (ao.contains("*")) {
            return true;
        }
        return containsOrigin(ao, origin);
    }

    /**
     * Adds CORS headers to the response if CORS is enabled and the request is a cross-origin request.
     *
     * <p>
     * This method checks if the request has an Origin header (indicating a cross-origin request). If CORS is enabled and the origin is
     * allowed, it adds the appropriate CORS headers to the response.
     * </p>
     *
     * <p>
     * <b>Note:</b> Headers are only added if they are not already present, allowing handlers to override defaults.
     * </p>
     *
     * @param requestHeaders
     *            The HTTP request (must implement CorsRequestInterface)
     * @param responseHeaders
     *            The response headers collection
     */
    public void addCorsHeaders(HeaderCollection requestHeaders, HeaderCollection responseHeaders) {

        // Check if this is a cross-origin request (has Origin header)
        String origin = requestHeaders.getValue(HTTPConstants.HEADER_REQUEST_ORIGIN);
        if (origin == null || origin.isEmpty()) {
            // Not a cross-origin request, no CORS headers needed
            return;
        }

        // Determine allowed origin
        String allowedOrigin = findAllowedOrigin(origin, requestHeaders, responseHeaders);

        // If no origin is allowed, don't add CORS headers
        if (allowedOrigin == null) {
            return;
        }

        // Add Access-Control-Allow-Origin
        HTTPHeader existingOriginHeader = responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        String finalAllowedOrigin = allowedOrigin;
        if (existingOriginHeader != null) {
            // Use existing header value if already set
            finalAllowedOrigin = existingOriginHeader.getValue();
        } else {
            responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN, allowedOrigin));
        }

        // Security validation: Never allow credentials with "*" origin (CORS specification violation)
        // Check if credentials are enabled (either via pattern-based rules, global setting, or manually set)
        HTTPHeader existingCredentialsHeader = responseHeaders.get(HTTPConstants.ACCESS_CONTROL_ALLOW_CREDENTIALS);
        boolean credentialsEnabled = isAllowCredentials(origin) || (existingCredentialsHeader != null && "true".equalsIgnoreCase(existingCredentialsHeader.getValue()));

        if (credentialsEnabled && "*".equals(finalAllowedOrigin)) {
            throw new IllegalStateException("Cannot set Access-Control-Allow-Credentials to true when Access-Control-Allow-Origin is \"*\". This violates CORS specification and is a security risk.");
        }

        // Add Access-Control-Allow-Credentials (only if not already set and credentials are enabled for this origin)
        if (isAllowCredentials(origin) && existingCredentialsHeader == null) {
            responseHeaders.add(new HTTPHeader(HTTPConstants.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true"));
        }

        // Add Access-Control-Expose-Headers
        String exposeHeadersValue = getExposeHeaders();
        if (exposeHeadersValue != null && !exposeHeadersValue.isEmpty()) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS) == null) {
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_EXPOSE_HEADERS, exposeHeadersValue));
            }
        }

        // Add Access-Control-Allow-Methods (typically for OPTIONS/preflight requests)
        Set<RequestMethod> allowMethodsValue = getAllowMethods();
        if (allowMethodsValue != null && !allowMethodsValue.isEmpty()) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS) == null) {
                // Convert Set<RequestMethod> to comma-separated string
                StringBuilder methodsBuilder = new StringBuilder();
                boolean first = true;
                for (RequestMethod method : allowMethodsValue) {
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
        if (isAllowHeadersFromRequest()) {
            String requestedHeaders = requestHeaders.getValue(HTTPConstants.HEADER_REQUEST_CONTROL_HEADERS);
            if (requestedHeaders != null && !requestedHeaders.isEmpty()) {
                if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS) == null) {
                    responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS, requestedHeaders));
                }
            }
        } else {
            String allowHeadersValue = getAllowHeaders();
            if (allowHeadersValue != null && !allowHeadersValue.isEmpty()) {
                if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS) == null) {
                    responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS, allowHeadersValue));
                }
            }
        }

        // Add Access-Control-Max-Age (typically for OPTIONS/preflight requests)
        // Note: HTTP header expects seconds, but we store milliseconds internally
        Long maxAgeValue = getMaxAge();
        if (maxAgeValue != null) {
            if (responseHeaders.get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE) == null) {
                // Convert milliseconds to seconds for the HTTP header
                long maxAgeSeconds = maxAgeValue / 1000;
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE, String.valueOf(maxAgeSeconds)));
            }
        }

        // Add Access-Control-Allow-Private-Network (for Private Network Access / PNA)
        // IMPORTANT: This header is ONLY relevant for preflight (OPTIONS) requests, not for actual GET/POST/PUT/DELETE requests.
        // The Access-Control-Request-Private-Network header is only sent by browsers during preflight requests.
        // We only set the response header if the request header is present, ensuring we only respond to preflight requests.
        // This is the correct behavior per CORS specification - we do NOT set this header unconditionally.
        // Check pattern-based rules first, then fall back to global setting
        if (isAllowPrivateNetworkFromRequest(origin)) {
            String requestedPrivateNetwork = requestHeaders.getValue(HTTPConstants.ACCESS_CONTROL_REQUEST_PRIVATE_NETWORK);
            if (requestedPrivateNetwork != null && !requestedPrivateNetwork.isEmpty()) {
                // Only set the header if it was requested (which only happens in preflight requests)
                // This ensures we don't unnecessarily set the header for non-preflight requests
                if (responseHeaders.get(HTTPConstants.ACCESS_CONTROL_ALLOW_PRIVATE_NETWORK) == null) {
                    responseHeaders.add(new HTTPHeader(HTTPConstants.ACCESS_CONTROL_ALLOW_PRIVATE_NETWORK, requestedPrivateNetwork));
                }
            }
        }
    }

    /**
     * @param origin
     * @param responseHeaders
     * @param requestHeaders
     * @return
     */
    protected String findAllowedOrigin(String origin, HeaderCollection requestHeaders, HeaderCollection responseHeaders) {
        String allowedOrigin = null;
        Set<String> ao = getAllowedOrigins();
        if (ao != null) {
            // If allowedOrigins contains "*", allow all; otherwise allow only if origin matches
            if (ao.contains("*")) {
                allowedOrigin = "*";
            } else if (containsOrigin(ao, origin)) {
                allowedOrigin = origin;
            } else {
                allowedOrigin = null;
            }
        } else {
            // No allowed origins configured -> forbid all Origin requests
            allowedOrigin = null;
        }
        return allowedOrigin;
    }

    /**
     * Handles an HTTP OPTIONS preflight request by preparing the complete response.
     *
     * <p>
     * This method should be called for OPTIONS requests when CORS is configured. It will:
     * <ul>
     * <li>Check if CORS is enabled (allowedOrigins is configured)</li>
     * <li>Validate that the origin is allowed</li>
     * <li>Ensure CORS headers are added via {@link #addCorsHeaders(HeaderCollection, HeaderCollection)}</li>
     * <li>Set the response code to 200 OK</li>
     * <li>Set Content-Length to 0</li>
     * </ul>
     * </p>
     *
     * <p>
     * <b>Note:</b> CORS headers are typically already added in configure() via addCorsHeaders(), but this method ensures they are present
     * before preparing the response.
     * </p>
     *
     * <p>
     * <b>Usage example:</b>
     * </p>
     *
     * <pre>{@code
     * if (request instanceof OptionsRequest) {
     *     CorsHandler corsHandler = server.getCorsHandler();
     *     if (corsHandler != null && corsHandler.answerOptionsRequest((OptionsRequest) request, response)) {
     *         response.getOutputStream(true).flush();
     *         return; // Request handled
     *     }
     *     // Fall through to normal handler processing if CORS is not configured or origin is not allowed
     * }
     * }</pre>
     *
     * @param request
     *            The OPTIONS request
     * @param response
     *            The HTTP response to prepare
     * @return true if the OPTIONS request was handled (CORS is enabled and origin is allowed), false otherwise
     * @throws java.io.IOException
     *             if an I/O error occurs while preparing the response
     */
    public boolean answerOptionsRequest(OptionsRequest request, HttpResponse response) throws java.io.IOException {
        // Check if CORS is enabled (allowedOrigins is configured)
        if (this.allowedOrigins == null || this.allowedOrigins.isEmpty()) {
            // CORS is not enabled - don't handle OPTIONS request
            return false;
        }

        // Check if origin is allowed
        if (!isOriginAllowed(request.getRequestHeaders())) {
            // Origin is not allowed - don't handle OPTIONS request
            return false;
        }

        // CORS is enabled and origin is allowed - prepare the OPTIONS response
        // Ensure CORS headers are added (they may already be added in configure(), but ensure they're present)
        addCorsHeaders(request.getRequestHeaders(), response.getResponseHeaders());

        // Set response code to 200 OK
        response.setResponseCode(ResponseCode.SUCCESS_OK);

        // Set Content-Length to 0 (OPTIONS requests have no body)
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, "0"));

        // Response is ready to be sent
        return true;
    }
}
