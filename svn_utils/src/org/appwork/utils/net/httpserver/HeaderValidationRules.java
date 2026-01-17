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

import java.util.HashMap;
import java.util.Map;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.HTTPHeader;

/**
 * Configuration class for header validation rules.
 * 
 * Defines mandatory headers (must be present, optionally with specific values matching regex patterns) and forbidden headers (must not be present or must not match regex patterns).
 */
public class HeaderValidationRules {
    /**
     * Map of mandatory headers. Key is header name (case-insensitive), value is regex pattern that the header value must match, or null if any value is acceptable.
     * Example: {"x-appwork": "1"} means header "x-appwork" must be present with value exactly "1".
     * Example: {"x-appwork": ".*"} means header "x-appwork" must be present with any value.
     * Example: {"x-appwork": null} means header "x-appwork" must be present, but value doesn't matter.
     * Example: {"x-version": "\\d+\\.\\d+"} means header "x-version" must match the regex pattern (e.g., "1.0", "2.5").
     */
    private final Map<String, String> mandatoryHeaders;
    
    /**
     * Map of forbidden headers. Key is header name (case-insensitive), value is regex pattern that the header value must not match, or null if header itself is forbidden regardless of value.
     * Example: {"sec-fetch-site": null} means header "sec-fetch-site" must not be present at all.
     * Example: {"sec-fetch-site": "cross-site"} means header "sec-fetch-site" must not have value matching "cross-site" (but other values are allowed).
     */
    private final Map<String, String> forbiddenHeaders;

    /**
     * Creates HeaderValidationRules with default values:
     * - Mandatory: {"x-appwork": "1"}
     * - Forbidden: common sec-fetch-* headers
     */
    public HeaderValidationRules() {
        this.mandatoryHeaders = new HashMap<String, String>();
        this.mandatoryHeaders.put(HTTPConstants.X_APPWORK, "1");
        
        this.forbiddenHeaders = new HashMap<String, String>();
        // Common sec-fetch-* headers that browsers send
        this.forbiddenHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, null);
        this.forbiddenHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, null);
        this.forbiddenHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_USER, null);
        this.forbiddenHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, null);
    }

    /**
     * Creates HeaderValidationRules with custom mandatory and forbidden headers.
     * 
     * @param mandatoryHeaders
     *            Map of mandatory headers. Key is header name, value is regex pattern that the header value must match, or null if any value is acceptable.
     * @param forbiddenHeaders
     *            Map of forbidden headers. Key is header name, value is regex pattern that the header value must not match, or null if header itself is forbidden.
     */
    public HeaderValidationRules(final Map<String, String> mandatoryHeaders, final Map<String, String> forbiddenHeaders) {
        this.mandatoryHeaders = mandatoryHeaders != null ? new HashMap<String, String>(mandatoryHeaders) : new HashMap<String, String>();
        this.forbiddenHeaders = forbiddenHeaders != null ? new HashMap<String, String>(forbiddenHeaders) : new HashMap<String, String>();
    }

    /**
     * Returns the map of mandatory headers.
     * 
     * @return Map where key is header name and value is regex pattern that the header value must match (or null if any value is acceptable)
     */
    public Map<String, String> getMandatoryHeaders() {
        return new HashMap<String, String>(this.mandatoryHeaders);
    }

    /**
     * Returns the map of forbidden headers.
     * 
     * @return Map where key is header name and value is regex pattern that the header value must not match (or null if header itself is forbidden)
     */
    public Map<String, String> getForbiddenHeaders() {
        return new HashMap<String, String>(this.forbiddenHeaders);
    }

    /**
     * Checks if header validation is enabled (i.e., if there are any rules defined).
     * 
     * @return true if there are mandatory or forbidden headers defined
     */
    public boolean isEnabled() {
        return !this.mandatoryHeaders.isEmpty() || !this.forbiddenHeaders.isEmpty();
    }

    /**
     * Validates the request headers against the configured rules.
     * 
     * Checks if all mandatory headers are present (and match their required patterns if specified)
     * and if no forbidden headers are present (or match their forbidden patterns if specified).
     * 
     * @param headers
     *            The request headers to validate
     * @return true if the request is allowed (all rules are satisfied), false otherwise
     */
    public boolean isRequestAllowed(final HeaderCollection headers) {
        if (headers == null) {
            return !this.isEnabled();
        }

        // Check mandatory headers
        for (final Map.Entry<String, String> entry : this.mandatoryHeaders.entrySet()) {
            final String headerName = entry.getKey();
            final String requiredPattern = entry.getValue();
            final HTTPHeader header = headers.get(headerName);

            if (header == null || header.getValue() == null) {
                return false;
            }

            // If requiredPattern is not null, check that the header value matches the regex pattern
            if (requiredPattern != null) {
                final String headerValue = header.getValue();
                if (!headerValue.matches(requiredPattern)) {
                    return false;
                }
            }
        }

        // Check forbidden headers
        for (final Map.Entry<String, String> entry : this.forbiddenHeaders.entrySet()) {
            final String headerName = entry.getKey();
            final String forbiddenPattern = entry.getValue();
            final HTTPHeader header = headers.get(headerName);

            if (header != null) {
                // If forbiddenPattern is null, header itself is forbidden regardless of value
                if (forbiddenPattern == null) {
                    return false;
                }
                // If forbiddenPattern is not null, check if header value matches the forbidden pattern
                final String headerValue = header.getValue();
                if (headerValue != null && headerValue.matches(forbiddenPattern)) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Gets a detailed error message describing why the request is not allowed.
     * Returns null if the request is allowed.
     * 
     * @param headers
     *            The request headers to validate
     * @return Error message string if validation fails, null if validation passes
     */
    public String getValidationError(final HeaderCollection headers) {
        if (headers == null) {
            if (this.isEnabled()) {
                return "Request headers are null but validation is enabled";
            }
            return null;
        }

        // Check mandatory headers
        for (final Map.Entry<String, String> entry : this.mandatoryHeaders.entrySet()) {
            final String headerName = entry.getKey();
            final String requiredPattern = entry.getValue();
            final HTTPHeader header = headers.get(headerName);

            if (header == null || header.getValue() == null) {
                return "Missing mandatory header: " + headerName;
            }

            // If requiredPattern is not null, check that the header value matches the regex pattern
            if (requiredPattern != null) {
                final String headerValue = header.getValue();
                if (!headerValue.matches(requiredPattern)) {
                    return "Invalid value for mandatory header " + headerName + ": value '" + headerValue + "' does not match required pattern '" + requiredPattern + "'";
                }
            }
        }

        // Check forbidden headers
        for (final Map.Entry<String, String> entry : this.forbiddenHeaders.entrySet()) {
            final String headerName = entry.getKey();
            final String forbiddenPattern = entry.getValue();
            final HTTPHeader header = headers.get(headerName);

            if (header != null) {
                // If forbiddenPattern is null, header itself is forbidden regardless of value
                if (forbiddenPattern == null) {
                    return "Forbidden header present: " + headerName;
                }
                // If forbiddenPattern is not null, check if header value matches the forbidden pattern
                final String headerValue = header.getValue();
                if (headerValue != null && headerValue.matches(forbiddenPattern)) {
                    return "Forbidden header value for " + headerName + ": value '" + headerValue + "' matches forbidden pattern '" + forbiddenPattern + "'";
                }
            }
        }

        return null;
    }

    /**
     * Returns a string representation of this header validation rules configuration for debugging purposes.
     * 
     * <p>
     * The format shows all configured rules in a readable format:
     * </p>
     * <pre>
     * HeaderValidationRules[
     *   mandatoryHeaders={x-appwork=1},
     *   forbiddenHeaders={sec-fetch-site=null, sec-fetch-mode=null, sec-fetch-user=null, sec-fetch-dest=null}
     * ]
     * </pre>
     * 
     * @return A string representation of this header validation rules configuration
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("HeaderValidationRules[");
        boolean hasContent = false;

        if (!this.mandatoryHeaders.isEmpty()) {
            sb.append("mandatoryHeaders=").append(this.mandatoryHeaders);
            hasContent = true;
        } else {
            sb.append("mandatoryHeaders={}");
            hasContent = true;
        }

        if (!this.forbiddenHeaders.isEmpty()) {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("forbiddenHeaders=").append(this.forbiddenHeaders);
        } else {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("forbiddenHeaders={}");
        }

        sb.append("]");
        return sb.toString();
    }
}

