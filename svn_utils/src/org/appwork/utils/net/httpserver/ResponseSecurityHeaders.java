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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;

/**
 * Configuration and management class for security headers that should be added to all HTTP responses.
 * 
 * <h2>Overview</h2>
 * This class provides a centralized way to configure and manage security-related HTTP response headers.
 * These headers protect against common web vulnerabilities and attacks such as clickjacking, MIME type
 * sniffing, XSS attacks, and information leakage through referrer headers.
 * 
 * <h2>Managed Security Headers</h2>
 * This class manages the following security headers:
 * <ul>
 *   <li><b>X-Content-Type-Options</b> - Prevents MIME type sniffing attacks</li>
 *   <li><b>X-Frame-Options OR Content-Security-Policy frame-ancestors</b> - Prevents clickjacking attacks (not both)</li>
 *   <li><b>Content-Security-Policy</b> - Provides fine-grained control over resource loading and framing</li>
 *   <li><b>Referrer-Policy</b> - Controls referrer information leakage</li>
 * </ul>
 * 
 * <h2>Important: X-Frame-Options vs CSP frame-ancestors</h2>
 * <p>
 * Both X-Frame-Options and Content-Security-Policy's frame-ancestors directive serve the same purpose:
 * preventing clickjacking attacks by controlling whether a page can be embedded in frames.
 * </p>
 * <h3>When Can Both Headers Be Used Together?</h3>
 * <p>
 * Both headers can be set simultaneously <b>only if they express compatible policies</b>. This class
 * automatically checks compatibility and handles conflicts appropriately.
 * </p>
 * 
 * <h4>Compatible Combinations</h4>
 * <ul>
 *   <li><b>DENY + 'none':</b> Both block all framing - compatible ✓</li>
 *   <li><b>SAMEORIGIN + 'self':</b> Both allow same-origin framing only - compatible ✓</li>
 * </ul>
 * 
 * <h4>Incompatible Combinations</h4>
 * <ul>
 *   <li><b>DENY + 'self':</b> DENY blocks all framing, 'self' allows same-origin - incompatible ✗</li>
 *   <li><b>SAMEORIGIN + 'none':</b> SAMEORIGIN allows same-origin, 'none' blocks all - incompatible ✗</li>
 *   <li><b>Any other combination:</b> Incompatible ✗</li>
 * </ul>
 * 
 * <h4>Why Compatibility Matters</h4>
 * <p>
 * According to the CSP Level 2 specification and browser implementations:
 * </p>
 * <ul>
 *   <li><b>Modern browsers (Chrome 40+, Firefox 39+, Safari 10+, Edge 15+):</b> CSP frame-ancestors
 *       <b>takes precedence</b> over X-Frame-Options. If both are present, X-Frame-Options is ignored.</li>
 *   <li><b>Older browsers:</b> May only support X-Frame-Options and ignore CSP frame-ancestors entirely.</li>
 * </ul>
 * <p>
 * <b>If headers are incompatible:</b> Modern browsers will enforce CSP (which might differ from X-Frame-Options),
 * while older browsers will enforce X-Frame-Options (which might differ from CSP). This creates unpredictable
 * behavior and potential security gaps.
 * </p>
 * <p>
 * <b>If headers are compatible:</b> Both express the same policy, so it doesn't matter which one the browser
 * enforces - the result is the same. This provides defense in depth: older browsers respect X-Frame-Options,
 * modern browsers respect CSP, and both policies are identical.
 * </p>
 * 
     * <h4>Our Implementation Strategy</h4>
     * <p>
     * This class enforces compatibility at configuration time:
     * </p>
     * <ul>
     *   <li><b>If headers are compatible:</b> Both are set, providing defense in depth for different browser versions.</li>
     *   <li><b>If headers are incompatible:</b> An {@link IllegalArgumentException} is thrown immediately when you try
     *       to set incompatible values. This ensures configuration errors are caught early (at configuration time),
     *       not silently corrected at runtime.</li>
     *   <li><b>Default:</b> Both X-Frame-Options: DENY and CSP frame-ancestors: 'none' are set (compatible policies).
     *       This provides maximum browser compatibility: older browsers respect X-Frame-Options, modern browsers respect CSP.</li>
     * </ul>
     * 
     * <p>
     * <b>Why fail fast?</b> If a developer configures incompatible headers, it's likely a mistake. By throwing
     * an exception immediately, we ensure the developer is aware of the issue and can fix it. Silent correction
     * at runtime could lead to unexpected behavior and security gaps.
     * </p>
     * 
     * <p>
     * <b>Best Practice:</b> By default, both X-Frame-Options and CSP frame-ancestors are set with compatible
     * policies (DENY + 'none'). This provides maximum browser compatibility: older browsers respect X-Frame-Options,
     * modern browsers respect CSP, and both policies are identical. If you need to customize, ensure both express
     * compatible policies (DENY + 'none' or SAMEORIGIN + 'self').
     * </p>
 * 
 * <h2>Default Configuration for REST API Servers</h2>
 * <p>
 * The default configuration is optimized for REST API servers that:
 * </p>
 * <ul>
 *   <li>Do not serve HTML pages or web content</li>
 *   <li>Do not load external resources (scripts, stylesheets, images, etc.)</li>
 *   <li>Should not be embeddable in frames (prevent clickjacking)</li>
 *   <li>Need protection against MIME type sniffing</li>
 *   <li>Should not leak referrer information</li>
 * </ul>
 * 
 * <h2>Security Headers Explained</h2>
 * 
 * <h3>X-Content-Type-Options: nosniff</h3>
 * <p>
 * <b>Purpose:</b> Prevents browsers from performing MIME type sniffing, which can lead to security vulnerabilities.
 * </p>
 * <p>
 * <b>Why it's needed:</b> Some browsers (especially older versions of Internet Explorer and Chrome) try to "guess"
 * the content type of a response by examining its content, even if the server explicitly sets a Content-Type header.
 * This behavior can be exploited: an attacker could upload a file with a harmless extension (like .txt) but
 * malicious content (like JavaScript), and the browser might execute it as JavaScript if it "sniffs" the content
 * type incorrectly.
 * </p>
 * <p>
 * <b>Default value:</b> "nosniff" - This is the only valid value for this header. It tells browsers to strictly
 * respect the Content-Type header sent by the server and never perform MIME type sniffing.
 * </p>
 * <p>
 * <b>Why this default:</b> REST APIs typically return JSON, XML, or plain text. There's no reason for browsers
 * to guess the content type - the server knows exactly what it's sending. This header ensures browsers trust
 * the server's Content-Type declaration.
 * </p>
 * 
     * <h3>Content-Security-Policy: default-src 'none'; frame-ancestors 'none'</h3>
     * <p>
     * <b>Purpose:</b> Provides comprehensive security policy for REST APIs with two main protections:
     * </p>
     * <ul>
     *   <li><b>default-src 'none':</b> Explicitly prevents any resource loading (scripts, stylesheets, images, fonts, etc.)</li>
     *   <li><b>frame-ancestors 'none':</b> Prevents the API from being embedded in frames (iframes, frames, embed, object tags),
     *       protecting against clickjacking attacks</li>
     * </ul>
     * 
     * <h4>Why default-src 'none'?</h4>
     * <p>
     * REST APIs typically return JSON, XML, or plain text - they don't serve HTML pages that load resources.
     * However, including "default-src 'none'" provides important security benefits:
     * </p>
     * <ul>
     *   <li><b>Explicit security policy:</b> Makes it crystal clear that no resources should be loaded. This is
     *       a security best practice - be explicit about what's allowed rather than relying on implicit behavior.</li>
     *   <li><b>Defense in depth:</b> If an API endpoint accidentally returns HTML content (e.g., due to a bug,
     *       misconfiguration, or error page), browsers won't load any resources from it. This prevents potential
     *       XSS attacks or resource loading vulnerabilities.</li>
     *   <li><b>Future-proofing:</b> If the API is later extended to serve HTML content (e.g., API documentation),
     *       developers must explicitly configure CSP directives, ensuring they think about security.</li>
     *   <li><b>Security scanners:</b> Many security scanning tools check for CSP headers. Having a complete CSP
     *       policy (even if minimal) shows that security headers are properly configured.</li>
     * </ul>
     * <p>
     * <b>Example scenario:</b> If an API endpoint accidentally returns an HTML error page instead of JSON (due to
     * a bug), and that HTML contains a &lt;script&gt; tag pointing to a malicious site, "default-src 'none'" will
     * prevent the browser from loading that script, protecting users even in error scenarios.
     * </p>
     * 
     * <h4>Why frame-ancestors 'none'?</h4>
     * <p>
     * Clickjacking is an attack where a malicious website embeds your API in an invisible or disguised frame,
     * tricking users into interacting with it. For example, an attacker could overlay a fake login form on top
     * of your API endpoint, making users think they're logging into the attacker's site when they're actually
     * interacting with your API.
     * </p>
     * <p>
     * REST APIs are typically consumed programmatically (via JavaScript fetch, XMLHttpRequest, or server-to-server
     * communication). There's no legitimate use case for embedding a REST API in a frame. Preventing framing
     * entirely eliminates clickjacking risks and ensures the API can only be accessed directly.
     * </p>
     * 
     * <h4>Why CSP instead of X-Frame-Options?</h4>
     * <p>
     * CSP frame-ancestors is the modern standard (CSP Level 2) and provides more flexibility. While X-Frame-Options
     * only supports DENY, SAMEORIGIN, or ALLOW-FROM (deprecated), CSP frame-ancestors supports more granular control.
     * However, for REST APIs, 'none' is sufficient and recommended.
     * </p>
     * 
     * <h4>Default CSP value: "default-src 'none'; frame-ancestors 'none'"</h4>
     * <p>
     * This is the complete default CSP policy. Both directives are essential:
     * </p>
     * <ul>
     *   <li><b>default-src 'none':</b> Prevents resource loading (defense in depth)</li>
     *   <li><b>frame-ancestors 'none':</b> Prevents clickjacking attacks</li>
     * </ul>
     * 
     * <h4>Adding additional CSP directives</h4>
     * <p>
     * If you need additional CSP directives (e.g., for an API that serves HTML documentation), you can add them
     * using {@link ContentSecurityPolicy#addDirective(String)}. However, you should then also adjust or remove
     * "default-src 'none'" to allow the necessary resources. For example:
     * </p>
     * <pre>{@code
     * ContentSecurityPolicy csp = new ContentSecurityPolicy();
     * csp.setFrameAncestors("'none'");
     * csp.addDirective("default-src 'self'"); // Replace 'none' with 'self' to allow same-origin resources
     * csp.addDirective("script-src 'self' 'unsafe-inline'");
     * }</pre>
 * 
 * <h3>Referrer-Policy: no-referrer</h3>
 * <p>
 * <b>Purpose:</b> Controls how much referrer information is sent with requests made from your API responses.
 * </p>
 * <p>
 * <b>Why it's needed:</b> By default, browsers send the full URL of the referring page in the Referer header
 * when making requests. This can leak sensitive information:
 * </p>
 * <ul>
 *   <li>API keys or tokens in URLs</li>
 *   <li>Session IDs in URLs</li>
 *   <li>Internal server names or paths</li>
 *   <li>User-specific information in query parameters</li>
 * </ul>
 * <p>
 * <b>Default value:</b> "no-referrer" - No referrer information is sent at all.
 * </p>
 * <p>
 * <b>Why this default:</b> REST APIs typically don't need referrer information. API clients (whether browsers
 * or server applications) don't rely on referrer headers for functionality. By setting "no-referrer", we
 * ensure that:
 * </p>
 * <ul>
 *   <li>No sensitive information leaks through referrer headers</li>
 *   <li>Privacy is maximized</li>
 *   <li>There's no risk of accidentally exposing internal URLs or API endpoints</li>
 * </ul>
 * <p>
 * <b>Alternative values:</b> If you need referrer information for analytics or security purposes, you can
 * use other values like "same-origin" (only send referrer for same-origin requests) or "strict-origin-when-cross-origin"
 * (send full referrer for same-origin, only origin for cross-origin, none when downgrading from HTTPS to HTTP).
 * </p>
 * 
 * <h2>Usage Examples</h2>
 * 
 * <h3>Default Configuration (Recommended for REST APIs)</h3>
 * <pre>{@code
 * HttpServer server = new HttpServer(8080);
 * // Uses default ResponseSecurityHeaders automatically
 * }</pre>
 * 
 * <h3>Custom Configuration</h3>
 * <pre>{@code
 * ResponseSecurityHeaders securityHeaders = new ResponseSecurityHeaders();
 * securityHeaders.setXFrameOptions(XFrameOptions.SAMEORIGIN); // Use X-Frame-Options instead of CSP
 * securityHeaders.setReferrerPolicy(ReferrerPolicy.SAME_ORIGIN);
 * server.setResponseSecurityHeaders(securityHeaders);
 * }</pre>
 * 
 * <h3>Custom CSP with Additional Directives</h3>
 * <pre>{@code
 * ResponseSecurityHeaders securityHeaders = new ResponseSecurityHeaders();
 * ContentSecurityPolicy csp = new ContentSecurityPolicy();
 * csp.setFrameAncestors("'self'"); // Allow same-origin framing
 * csp.addDirective("default-src 'self'"); // Additional CSP directive
 * securityHeaders.setContentSecurityPolicy(csp);
 * server.setResponseSecurityHeaders(securityHeaders);
 * }</pre>
 * 
 * <h3>Disable Security Headers</h3>
 * <pre>{@code
 * server.setResponseSecurityHeaders(null); // Disables all default security headers
 * }</pre>
 * 
 * <h2>Security Best Practices</h2>
 * <ul>
 *   <li><b>Always use security headers:</b> Even if you think your API is "internal only", security headers
 *       provide defense in depth and protect against various attack vectors.</li>
 *   <li><b>Don't disable without good reason:</b> The default configuration is carefully chosen for REST APIs.
 *       Only disable or modify if you have a specific requirement.</li>
 *   <li><b>Test your configuration:</b> Use tools like securityheaders.com or browser developer tools to
 *       verify that headers are being sent correctly.</li>
 *   <li><b>Keep headers consistent:</b> Don't mix X-Frame-Options and CSP frame-ancestors - choose one
 *       approach and stick with it.</li>
 * </ul>
 * 
 * <h2>Browser Compatibility</h2>
 * <ul>
 *   <li><b>X-Content-Type-Options:</b> Supported by all modern browsers (Chrome, Firefox, Edge, Safari)</li>
 *   <li><b>Content-Security-Policy:</b> Supported by all modern browsers. frame-ancestors requires CSP Level 2
 *       (supported since Chrome 40, Firefox 39, Safari 10, Edge 15)</li>
 *   <li><b>X-Frame-Options:</b> Supported by all modern browsers (including older versions)</li>
 *   <li><b>Referrer-Policy:</b> Supported by all modern browsers (Chrome 56+, Firefox 50+, Safari 11.1+, Edge 79+)</li>
 * </ul>
 * 
 * @author AppWork
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options">MDN: X-Content-Type-Options</a>
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy">MDN: Content-Security-Policy</a>
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options">MDN: X-Frame-Options</a>
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy">MDN: Referrer-Policy</a>
 * @see <a href="https://scotthelme.co.uk/hardening-your-http-response-headers/">Hardening HTTP Response Headers</a>
 */
public class ResponseSecurityHeaders {
    private XContentTypeOptions    xContentTypeOptions;
    private XFrameOptions          xFrameOptions;
    private ContentSecurityPolicy  contentSecurityPolicy;
    private ReferrerPolicy         referrerPolicy;

    /**
     * Creates a new ResponseSecurityHeaders with default security values optimized for REST API servers.
     * 
     * <h3>Default Security Headers</h3>
     * <ul>
     *   <li><b>X-Content-Type-Options: nosniff</b> - Prevents MIME type sniffing attacks</li>
     *   <li><b>X-Frame-Options: DENY</b> - Prevents clickjacking attacks (for older browser compatibility)</li>
     *   <li><b>Content-Security-Policy: default-src 'none'; frame-ancestors 'none'</b> - Prevents resource loading and clickjacking (modern standard)</li>
     *   <li><b>Referrer-Policy: no-referrer</b> - Prevents referrer information leakage</li>
     * </ul>
     * 
     * <h3>Why These Defaults?</h3>
     * <p>
     * These defaults are specifically chosen for REST API servers that:
     * </p>
     * <ul>
     *   <li><b>Don't serve HTML pages:</b> REST APIs typically return JSON, XML, or plain text. They don't
     *       need complex CSP directives like script-src, style-src, or img-src because there's no HTML content
     *       that loads resources.</li>
     *   <li><b>Should not be framable:</b> There's no legitimate use case for embedding a REST API in an iframe.
     *       APIs are consumed programmatically, not displayed in web pages. Blocking all framing prevents
     *       clickjacking attacks entirely.</li>
     *   <li><b>Need MIME type protection:</b> Even though APIs typically return JSON/XML, MIME type sniffing
     *       protection is important for defense in depth. If an API endpoint accidentally returns content with
     *       a wrong Content-Type, browsers won't try to "fix" it by sniffing.</li>
     *   <li><b>Should protect privacy:</b> REST APIs often contain sensitive data in URLs (API keys, tokens,
     *       session IDs). By setting "no-referrer", we ensure this information never leaks through referrer headers.</li>
     * </ul>
     * 
     * <h3>Why Both X-Frame-Options and CSP frame-ancestors?</h3>
     * <p>
     * Both headers are set by default (X-Frame-Options: DENY and CSP frame-ancestors: 'none') because they express
     * compatible policies and provide defense in depth:
     * </p>
     * <ul>
     *   <li><b>X-Frame-Options:</b> Supported by all browsers, including older versions that don't support CSP Level 2</li>
     *   <li><b>CSP frame-ancestors:</b> Modern standard (CSP Level 2, supported since 2015) that takes precedence in modern browsers</li>
     *   <li><b>Compatible policies:</b> DENY and 'none' both block all framing, so there's no conflict</li>
     * </ul>
     * <p>
     * This ensures maximum compatibility: older browsers respect X-Frame-Options, modern browsers respect CSP,
     * and both policies are identical, providing consistent protection across all browser versions.
     * </p>
     * 
     * <h3>What's NOT Included (and Why)</h3>
     * <ul>
     *   <li><b>CSP default-src 'none':</b> The default CSP includes "default-src 'none'" to explicitly state that
     *       no resources should be loaded. This is appropriate for REST APIs because they don't serve HTML content
     *       that would load resources (scripts, stylesheets, images, etc.). This directive makes the security
     *       policy explicit and defensive - even if an API endpoint accidentally returns HTML content, browsers
     *       won't load any resources from it.</li>
     *   <li><b>X-XSS-Protection:</b> This header is deprecated and not recommended for modern applications.
     *       CSP provides better XSS protection, and for REST APIs (which don't serve HTML), XSS protection
     *       is not relevant.</li>
     * </ul>
     * 
     * <h3>When to Customize</h3>
     * <p>
     * You should customize these defaults if:
     * </p>
     * <ul>
     *   <li>Your API serves HTML documentation or web interfaces (add appropriate CSP directives)</li>
     *   <li>You need to allow same-origin framing (change both X-Frame-Options to SAMEORIGIN and CSP frame-ancestors to 'self')</li>
     *   <li>You need referrer information for analytics (change Referrer-Policy)</li>
     *   <li>You want to use only CSP (set X-Frame-Options to null) or only X-Frame-Options (set CSP to null)</li>
     * </ul>
     * 
     * @see #ResponseSecurityHeaders(XContentTypeOptions, XFrameOptions, ContentSecurityPolicy, ReferrerPolicy)
     * @see #addSecurityHeaders(HeaderCollection)
     */
    public ResponseSecurityHeaders() {
        this.xContentTypeOptions = XContentTypeOptions.NOSNIFF;
        // Set X-Frame-Options: DENY for older browser compatibility
        // This is compatible with CSP frame-ancestors 'none' (both block all framing)
        this.xFrameOptions = XFrameOptions.DENY;
        // For REST APIs: default-src 'none' explicitly prevents any resource loading
        // frame-ancestors 'none' prevents framing (clickjacking protection)
        // Both X-Frame-Options and CSP frame-ancestors are set for maximum browser compatibility
        final ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'none'");
        csp.addDirective("default-src 'none'");
        this.contentSecurityPolicy = csp;
        this.referrerPolicy = ReferrerPolicy.NO_REFERRER;
    }

    /**
     * Creates a new ResponseSecurityHeaders with custom values.
     * 
     * <p>
     * <b>Note:</b> Compatibility validation between X-Frame-Options and CSP frame-ancestors is performed
     * when headers are added to responses (in {@link #addSecurityHeaders(HeaderCollection)}), not at
     * construction time. This allows configuration to be set up without immediate validation.
     * </p>
     * 
     * @param xContentTypeOptions
     *            The X-Content-Type-Options value, or null to disable
     * @param xFrameOptions
     *            The X-Frame-Options value, or null to disable
     * @param contentSecurityPolicy
     *            The Content-Security-Policy value, or null to disable
     * @param referrerPolicy
     *            The Referrer-Policy value, or null to disable
     */
    public ResponseSecurityHeaders(final XContentTypeOptions xContentTypeOptions, final XFrameOptions xFrameOptions, final ContentSecurityPolicy contentSecurityPolicy, final ReferrerPolicy referrerPolicy) {
        this.xContentTypeOptions = xContentTypeOptions;
        this.xFrameOptions = xFrameOptions;
        this.contentSecurityPolicy = contentSecurityPolicy;
        this.referrerPolicy = referrerPolicy;
    }

    /**
     * Checks if X-Frame-Options and CSP frame-ancestors are compatible (i.e., they express the same policy).
     * 
     * <p>
     * Compatibility rules:
     * </p>
     * <ul>
     *   <li><b>DENY</b> is compatible with <b>'none'</b> - both block all framing</li>
     *   <li><b>SAMEORIGIN</b> is compatible with <b>'self'</b> - both allow same-origin framing only</li>
     *   <li>All other combinations are incompatible</li>
     * </ul>
     * 
     * @param xFrameOptions
     *            The X-Frame-Options value, or null
     * @param cspFrameAncestors
     *            The CSP frame-ancestors value, or null
     * @return true if both are null, or if they express compatible policies; false if they conflict
     */
    private static boolean areFramingHeadersCompatible(final XFrameOptions xFrameOptions, final String cspFrameAncestors) {
        // If both are null, they're compatible (neither is set)
        if (xFrameOptions == null && (cspFrameAncestors == null || cspFrameAncestors.trim().isEmpty())) {
            return true;
        }
        
        // If only one is set, they're compatible (no conflict)
        if (xFrameOptions == null || cspFrameAncestors == null || cspFrameAncestors.trim().isEmpty()) {
            return true;
        }
        
        // Check for compatible combinations
        final String cspValue = cspFrameAncestors.trim();
        
        // DENY is compatible with 'none'
        if (xFrameOptions == XFrameOptions.DENY) {
            return "'none'".equals(cspValue);
        }
        
        // SAMEORIGIN is compatible with 'self'
        if (xFrameOptions == XFrameOptions.SAMEORIGIN) {
            return "'self'".equals(cspValue);
        }
        
        // ALLOW_FROM is not compatible with any CSP value (it's deprecated anyway)
        // Any other combination is incompatible
        return false;
    }


    /**
     * Adds security headers to the response headers collection if they are not already present.
     * 
     * <h3>How It Works</h3>
     * <p>
     * This method iterates through all configured security headers and adds them to the response headers
     * collection, but only if they are not already present. This design allows:
     * </p>
     * <ul>
     *   <li><b>Handler override:</b> Individual request handlers can set their own security headers before
     *       this method is called, and those will take precedence.</li>
     *   <li><b>Default behavior:</b> If no handler sets security headers, the defaults from this configuration
     *       will be applied automatically.</li>
     *   <li><b>Selective override:</b> Handlers can override individual headers while keeping others.</li>
     * </ul>
     * 
     * <h3>Header Processing Order</h3>
     * <ol>
     *   <li><b>X-Content-Type-Options:</b> Added if configured and not already present</li>
     *   <li><b>X-Frame-Options:</b> Added if configured and not already present</li>
     *   <li><b>Content-Security-Policy:</b> Added if configured and not already present</li>
     *   <li><b>Referrer-Policy:</b> Added if configured and not already present</li>
     * </ol>
     * 
     * <h3>Compatibility Check: X-Frame-Options vs CSP frame-ancestors</h3>
     * <p>
     * Both X-Frame-Options and CSP's frame-ancestors directive serve the same purpose: preventing clickjacking.
     * They can be used together if they express compatible policies:
     * </p>
     * <ul>
     *   <li><b>Compatible:</b> DENY + 'none' (both block all framing)</li>
     *   <li><b>Compatible:</b> SAMEORIGIN + 'self' (both allow same-origin framing only)</li>
     *   <li><b>Incompatible:</b> Any other combination (e.g., DENY + 'self', SAMEORIGIN + 'none')</li>
     * </ul>
     * <p>
     * <b>Important:</b> Compatibility validation is performed when headers are written to the response
     * (in {@link HttpServer#validateResponseHeaders(HttpResponse)}), not when headers are added. This ensures
     * that validation happens on the final headers that will be sent, allowing handlers to override headers
     * while still ensuring compatibility.
     * </p>
     * <p>
     * <b>Why allow compatible combinations?</b> Setting both headers can provide defense in depth:
     * older browsers that don't support CSP Level 2 will respect X-Frame-Options, while modern browsers
     * will respect CSP frame-ancestors. As long as both express the same policy, there's no conflict.
     * </p>
     * 
     * <h3>Example: Handler Override</h3>
     * <pre>{@code
     * // In a request handler:
     * response.getResponseHeaders().add(new HTTPHeader(
     *     HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS, 
     *     "SAMEORIGIN"
     * ));
     * // If the default CSP has frame-ancestors 'self', both will be set (compatible)
     * // If the default CSP has frame-ancestors 'none', validation will fail when headers are written
     * }</pre>
     * 
     * <h3>Example: Selective Override</h3>
     * <pre>{@code
     * // In a request handler:
     * response.getResponseHeaders().add(new HTTPHeader(
     *     HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY, 
     *     "same-origin"
     * ));
     * // Other security headers (X-Content-Type-Options, CSP) will still be added
     * }</pre>
     * 
     * @param responseHeaders
     *            The HeaderCollection to add security headers to. Must not be null (method returns early if null).
     *            Headers are only added if they are not already present in this collection.
     * 
     * @see #ResponseSecurityHeaders()
     * @see #validateResponseHeaders(HeaderCollection)
     * @see HttpServer#validateResponseHeaders(HttpResponse)
     * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors">MDN: CSP frame-ancestors</a>
     */
    public void addSecurityHeaders(final HeaderCollection responseHeaders) {
        if (responseHeaders == null) {
            return;
        }

        // Note: Validation is performed later when headers are written (in HttpServer.validateResponseHeaders),
        // not here. This allows headers to be set and modified before final validation.

        // X-Content-Type-Options
        XContentTypeOptions xContentTypeOptionsValue = getXContentTypeOptions();
        if (xContentTypeOptionsValue != null && responseHeaders.get(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS) == null) {
            responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS, xContentTypeOptionsValue.getValue()));
        }

        // X-Frame-Options and CSP frame-ancestors
        XFrameOptions xFrameOptionsValue = getXFrameOptions();
        final boolean useXFrameOptions = xFrameOptionsValue != null;
        ContentSecurityPolicy contentSecurityPolicyValue = getContentSecurityPolicy();
        final boolean useCSP = contentSecurityPolicyValue != null;

        // Add X-Frame-Options if configured
        if (useXFrameOptions && responseHeaders.get(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS) == null) {
            responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS, xFrameOptionsValue.getValue()));
        }

        // Content-Security-Policy
        if (useCSP && responseHeaders.get(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY) == null) {
            final String cspValue = contentSecurityPolicyValue.toHeaderString();
            if (cspValue != null && !cspValue.isEmpty()) {
                responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY, cspValue));
            }
        }

        // Referrer-Policy
        ReferrerPolicy referrerPolicyValue = getReferrerPolicy();
        if (referrerPolicyValue != null && responseHeaders.get(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY) == null) {
            responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY, referrerPolicyValue.getValue()));
        }
    }

    /**
     * Validates the response headers for compatibility issues.
     * 
     * <p>
     * This method checks if X-Frame-Options and CSP frame-ancestors are compatible when both are present
     * in the response headers. This validation is performed on the actual headers that will be sent,
     * not on the configuration, allowing handlers to override headers while still ensuring compatibility.
     * </p>
     * 
     * @param responseHeaders
     *            The HeaderCollection containing the response headers to validate
     * @throws IllegalArgumentException
     *             if X-Frame-Options and CSP frame-ancestors are both present but incompatible
     */
    public void validateResponseHeaders(final HeaderCollection responseHeaders) {
        if (responseHeaders == null) {
            return;
        }

        // Check if both X-Frame-Options and CSP frame-ancestors are present in the actual response headers
        final HTTPHeader xFrameOptionsHeader = responseHeaders.get(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
        final HTTPHeader cspHeader = responseHeaders.get(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);

        if (xFrameOptionsHeader == null || cspHeader == null || cspHeader.getValue() == null) {
            return; // No conflict if one or both are not present
        }

        // Extract frame-ancestors from CSP header value
        final String cspValue = cspHeader.getValue();
        String cspFrameAncestors = null;
        
        // Parse CSP header to find frame-ancestors directive
        // Format: "directive1 value1; directive2 value2; frame-ancestors 'none'"
        final String[] directives = cspValue.split(";");
        for (final String directive : directives) {
            final String trimmed = directive.trim();
            if (trimmed.startsWith("frame-ancestors")) {
                // Extract the value after "frame-ancestors"
                final int spaceIndex = trimmed.indexOf(' ');
                if (spaceIndex > 0) {
                    cspFrameAncestors = trimmed.substring(spaceIndex + 1).trim();
                }
                break;
            }
        }

        if (cspFrameAncestors == null || cspFrameAncestors.isEmpty()) {
            return; // No frame-ancestors in CSP, no conflict
        }

        // Parse X-Frame-Options value
        final String xFrameOptionsValue = xFrameOptionsHeader.getValue();
        if (xFrameOptionsValue == null) {
            return;
        }

        final XFrameOptions xFrameOptions = XFrameOptions.fromString(xFrameOptionsValue);
        if (xFrameOptions == null) {
            return; // Unknown X-Frame-Options value, skip validation
        }

        // Validate compatibility using existing method
        if (!areFramingHeadersCompatible(xFrameOptions, cspFrameAncestors)) {
            throw new IllegalArgumentException("Incompatible framing headers in response: X-Frame-Options=" + xFrameOptionsValue + " conflicts with CSP frame-ancestors=" + cspFrameAncestors + ". Compatible combinations: DENY + 'none', SAMEORIGIN + 'self'. Use only one header or ensure both express the same policy.");
        }
    }

    /**
     * Returns the X-Content-Type-Options value.
     * 
     * @return The X-Content-Type-Options value, or null if this header is disabled.
     *         Default is {@link XContentTypeOptions#NOSNIFF}.
     */
    public XContentTypeOptions getXContentTypeOptions() {
        return this.xContentTypeOptions;
    }

    /**
     * Sets the X-Content-Type-Options value.
     * 
     * <p>
     * This header prevents browsers from performing MIME type sniffing. The only valid value is
     * {@link XContentTypeOptions#NOSNIFF}, which tells browsers to strictly respect the Content-Type
     * header sent by the server.
     * </p>
     * 
     * <p>
     * <b>When to disable:</b> Only disable this header if you have a specific requirement that
     * conflicts with it. For REST APIs, there's no reason to disable it - APIs know exactly what
     * content type they're sending.
     * </p>
     * 
     * @param xContentTypeOptions
     *            The X-Content-Type-Options value (typically {@link XContentTypeOptions#NOSNIFF}),
     *            or null to disable this header entirely.
     */
    public void setXContentTypeOptions(final XContentTypeOptions xContentTypeOptions) {
        this.xContentTypeOptions = xContentTypeOptions;
    }

    /**
     * Returns the X-Frame-Options value.
     * 
     * @return The X-Frame-Options value, or null if this header is disabled.
     *         Default is {@link XFrameOptions#DENY} (set together with CSP frame-ancestors 'none' for maximum browser compatibility).
     */
    public XFrameOptions getXFrameOptions() {
        return this.xFrameOptions;
    }

    /**
     * Sets the X-Frame-Options value.
     * 
     * <p>
     * This header prevents clickjacking attacks by controlling whether the API can be embedded
     * in frames. Valid values are:
     * </p>
     * <ul>
     *   <li>{@link XFrameOptions#DENY} - Cannot be embedded in any frame</li>
     *   <li>{@link XFrameOptions#SAMEORIGIN} - Can only be embedded in frames from the same origin</li>
     *   <li>{@link XFrameOptions#ALLOW_FROM} - Deprecated, not widely supported</li>
     * </ul>
     * 
     * <p>
     * <b>Important - Compatibility Check:</b> If CSP frame-ancestors is also configured, both headers
     * must express compatible policies. Compatible combinations:
     * </p>
     * <ul>
     *   <li>DENY + 'none' (both block all framing)</li>
     *   <li>SAMEORIGIN + 'self' (both allow same-origin framing only)</li>
     * </ul>
     * <p>
     * Compatibility validation is performed when headers are added to responses (in
     * {@link #addSecurityHeaders(HeaderCollection)}), not when this setter is called. If incompatible
     * values are configured, an {@link IllegalArgumentException} is thrown when headers are added to
     * a response.
     * </p>
     * 
     * <p>
     * <b>Default behavior:</b> By default, this is set to {@link XFrameOptions#DENY} together with
     * CSP frame-ancestors 'none' for maximum browser compatibility. Both headers express the same
     * policy (block all framing), providing defense in depth: older browsers respect X-Frame-Options,
     * modern browsers respect CSP, and both policies are identical.
     * </p>
     * 
     * <p>
     * <b>When to disable:</b> Set to null if you want to use only CSP frame-ancestors (modern standard only).
     * However, keeping both is recommended for maximum compatibility across all browser versions.
     * </p>
     * 
     * @param xFrameOptions
     *            The X-Frame-Options value ({@link XFrameOptions#DENY}, {@link XFrameOptions#SAMEORIGIN}),
     *            or null to disable this header (use CSP frame-ancestors instead).
     */
    public void setXFrameOptions(final XFrameOptions xFrameOptions) {
        this.xFrameOptions = xFrameOptions;
    }

    /**
     * Returns the Content-Security-Policy value.
     * 
     * @return The Content-Security-Policy value, or null if this header is disabled.
     *         Default is a CSP with frame-ancestors 'none'.
     */
    public ContentSecurityPolicy getContentSecurityPolicy() {
        return this.contentSecurityPolicy;
    }

    /**
     * Sets the Content-Security-Policy value.
     * 
     * <p>
     * Content-Security-Policy (CSP) provides fine-grained control over resource loading and framing.
     * For REST APIs, the default CSP only includes frame-ancestors to prevent clickjacking, but you
     * can add additional directives if needed.
     * </p>
     * 
     * <p>
     * <b>Default CSP:</b> The default CSP is "default-src 'none'; frame-ancestors 'none'":
     * </p>
     * <ul>
     *   <li><b>default-src 'none':</b> Explicitly prevents any resource loading. This is appropriate for REST APIs
     *       that don't serve HTML content. Even if HTML is accidentally returned, no resources will be loaded.</li>
     *   <li><b>frame-ancestors 'none':</b> Prevents all framing, equivalent to X-Frame-Options: DENY but uses the
     *       modern CSP standard.</li>
     * </ul>
     * 
     * <p>
     * <b>Adding CSP directives:</b> You can add additional CSP directives for APIs that serve HTML
     * content (e.g., API documentation):
     * </p>
     * <pre>{@code
     * ContentSecurityPolicy csp = new ContentSecurityPolicy();
     * csp.setFrameAncestors("'none'");
     * csp.addDirective("default-src 'self'");
     * csp.addDirective("script-src 'self' 'unsafe-inline'");
     * securityHeaders.setContentSecurityPolicy(csp);
     * }</pre>
     * 
     * <p>
     * <b>Important - Compatibility Check:</b> If X-Frame-Options is also configured, both headers
     * must express compatible policies. Compatible combinations:
     * </p>
     * <ul>
     *   <li>DENY + 'none' (both block all framing)</li>
     *   <li>SAMEORIGIN + 'self' (both allow same-origin framing only)</li>
     * </ul>
     * <p>
     * Compatibility validation is performed when headers are added to responses (in
     * {@link #addSecurityHeaders(HeaderCollection)}), not when this setter is called. If incompatible
     * values are configured, an {@link IllegalArgumentException} is thrown when headers are added
     * to a response.
     * </p>
     * 
     * <p>
     * <b>When to disable:</b> Only disable CSP if you're using X-Frame-Options instead and don't
     * need any CSP directives. However, it's recommended to keep CSP with at least frame-ancestors
     * for modern browser support.
     * </p>
     * 
     * @param contentSecurityPolicy
     *            The Content-Security-Policy value, or null to disable this header entirely.
     *            If null and X-Frame-Options is also null, no framing protection will be applied.
     */
    public void setContentSecurityPolicy(final ContentSecurityPolicy contentSecurityPolicy) {
        this.contentSecurityPolicy = contentSecurityPolicy;
    }

    /**
     * Returns the Referrer-Policy value.
     * 
     * @return The Referrer-Policy value, or null if this header is disabled.
     *         Default is {@link ReferrerPolicy#NO_REFERRER}.
     */
    public ReferrerPolicy getReferrerPolicy() {
        return this.referrerPolicy;
    }

    /**
     * Sets the Referrer-Policy value.
     * 
     * <p>
     * Referrer-Policy controls how much referrer information is sent with requests. This is important
     * for REST APIs because:
     * </p>
     * <ul>
     *   <li>API URLs often contain sensitive information (API keys, tokens, session IDs)</li>
     *   <li>Referrer headers can leak internal server names and paths</li>
     *   <li>Privacy regulations may require limiting referrer information</li>
     * </ul>
     * 
     * <p>
     * <b>Available policies:</b>
     * </p>
     * <ul>
     *   <li>{@link ReferrerPolicy#NO_REFERRER} - No referrer information sent (default, most private)</li>
     *   <li>{@link ReferrerPolicy#NO_REFERRER_WHEN_DOWNGRADE} - No referrer when downgrading HTTPS→HTTP</li>
     *   <li>{@link ReferrerPolicy#ORIGIN} - Only send origin (e.g., "https://api.example.com")</li>
     *   <li>{@link ReferrerPolicy#ORIGIN_WHEN_CROSS_ORIGIN} - Full referrer same-origin, origin only cross-origin</li>
     *   <li>{@link ReferrerPolicy#SAME_ORIGIN} - Full referrer same-origin, none cross-origin</li>
     *   <li>{@link ReferrerPolicy#STRICT_ORIGIN} - Origin only, none when downgrading</li>
     *   <li>{@link ReferrerPolicy#STRICT_ORIGIN_WHEN_CROSS_ORIGIN} - Full same-origin, origin cross-origin, none when downgrading</li>
     *   <li>{@link ReferrerPolicy#UNSAFE_URL} - Always send full referrer (least private, not recommended)</li>
     * </ul>
     * 
     * <p>
     * <b>Default: NO_REFERRER</b> This is the most private option and is recommended for REST APIs
     * because:
     * </p>
     * <ul>
     *   <li>APIs don't need referrer information for functionality</li>
     *   <li>It prevents all information leakage through referrer headers</li>
     *   <li>It protects sensitive data in URLs</li>
     * </ul>
     * 
     * <p>
     * <b>When to change:</b> Consider changing to {@link ReferrerPolicy#SAME_ORIGIN} or
     * {@link ReferrerPolicy#STRICT_ORIGIN_WHEN_CROSS_ORIGIN} if:
     * </p>
     * <ul>
     *   <li>You need referrer information for analytics or security monitoring</li>
     *   <li>You want to allow same-origin referrer information but block cross-origin</li>
     *   <li>You're serving HTML content that benefits from referrer information</li>
     * </ul>
     * 
     * @param referrerPolicy
     *            The Referrer-Policy value, or null to disable this header entirely.
     *            Recommended: {@link ReferrerPolicy#NO_REFERRER} for maximum privacy.
     */
    public void setReferrerPolicy(final ReferrerPolicy referrerPolicy) {
        this.referrerPolicy = referrerPolicy;
    }

    /**
     * Returns a string representation of this security headers configuration for debugging purposes.
     * 
     * <p>
     * The format shows all configured headers in a readable format:
     * </p>
     * <pre>
     * ResponseSecurityHeaders[
     *   X-Content-Type-Options=nosniff,
     *   X-Frame-Options=null,
     *   Content-Security-Policy=ContentSecurityPolicy[frame-ancestors='none', directives=['default-src 'none']],
     *   Referrer-Policy=no-referrer
     * ]
     * </pre>
     * 
     * @return A string representation of this security headers configuration
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("ResponseSecurityHeaders[");
        boolean hasContent = false;

        XContentTypeOptions xContentTypeOptionsValue = getXContentTypeOptions();
        if (xContentTypeOptionsValue != null) {
            sb.append("X-Content-Type-Options=").append(xContentTypeOptionsValue.getValue());
            hasContent = true;
        } else {
            sb.append("X-Content-Type-Options=null");
            hasContent = true;
        }

        XFrameOptions xFrameOptionsValue = getXFrameOptions();
        if (xFrameOptionsValue != null) {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("X-Frame-Options=").append(xFrameOptionsValue.getValue());
            hasContent = true;
        } else {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("X-Frame-Options=null");
            hasContent = true;
        }

        ContentSecurityPolicy contentSecurityPolicyValue = getContentSecurityPolicy();
        if (contentSecurityPolicyValue != null) {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("Content-Security-Policy=").append(contentSecurityPolicyValue.toString());
            hasContent = true;
        } else {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("Content-Security-Policy=null");
            hasContent = true;
        }

        ReferrerPolicy referrerPolicyValue = getReferrerPolicy();
        if (referrerPolicyValue != null) {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("Referrer-Policy=").append(referrerPolicyValue.getValue());
            hasContent = true;
        } else {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("Referrer-Policy=null");
        }

        sb.append("]");
        return sb.toString();
    }
}

