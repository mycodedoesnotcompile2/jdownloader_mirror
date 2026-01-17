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

import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;

/**
 * Builder class for Content-Security-Policy header values.
 * 
 * This class helps build CSP policies with support for frame-ancestors directive
 * (which can replace X-Frame-Options).
 * 
 * @author AppWork
 */
public class ContentSecurityPolicy {
    private String              frameAncestors;
    private final List<String> additionalDirectives = new ArrayList<String>();

    /**
     * Creates a new ContentSecurityPolicy builder.
     */
    public ContentSecurityPolicy() {
    }

    /**
     * Sets the frame-ancestors directive. This replaces X-Frame-Options.
     * Common values:
     * - "none" (equivalent to X-Frame-Options: DENY)
     * - "'self'" (equivalent to X-Frame-Options: SAMEORIGIN)
     * - "'self' https://example.com" (allow self and example.com)
     * 
     * @param frameAncestors
     *            The frame-ancestors value, or null to omit this directive
     */
    public void setFrameAncestors(final String frameAncestors) {
        this.frameAncestors = frameAncestors;
    }

    /**
     * Returns the frame-ancestors directive value.
     * 
     * @return The frame-ancestors value, or null if not set
     */
    public String getFrameAncestors() {
        return this.frameAncestors;
    }

    /**
     * Adds an additional CSP directive. The directive should be in the format "directive-name value1 value2".
     * Examples:
     * - "default-src 'self'"
     * - "script-src 'self' 'unsafe-inline'"
     * - "style-src 'self' https://cdn.example.com"
     * 
     * @param directive
     *            The CSP directive string (e.g., "default-src 'self'")
     */
    public void addDirective(final String directive) {
        if (StringUtils.isNotEmpty(directive)) {
            this.additionalDirectives.add(directive);
        }
    }

    /**
     * Returns all additional directives.
     * 
     * @return List of additional CSP directives
     */
    public List<String> getAdditionalDirectives() {
        return new ArrayList<String>(this.additionalDirectives);
    }

    /**
     * Builds the CSP header value string.
     * 
     * @return The CSP header value, or null if no directives are set
     */
    public String toHeaderString() {
        final List<String> parts = new ArrayList<String>();

        // Add frame-ancestors if set
        if (StringUtils.isNotEmpty(this.frameAncestors)) {
            parts.add("frame-ancestors " + this.frameAncestors);
        }

        // Add additional directives
        parts.addAll(this.additionalDirectives);

        if (parts.isEmpty()) {
            return null;
        }

        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parts.size(); i++) {
            if (i > 0) {
                sb.append("; ");
            }
            sb.append(parts.get(i));
        }
        return sb.toString();
    }

    /**
     * Creates a default CSP policy with frame-ancestors 'none' (equivalent to X-Frame-Options: DENY).
     * 
     * @return A ContentSecurityPolicy with frame-ancestors 'none'
     */
    public static ContentSecurityPolicy defaultPolicy() {
        final ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'none'");
        return csp;
    }

    /**
     * Creates a CSP policy with frame-ancestors 'self' (equivalent to X-Frame-Options: SAMEORIGIN).
     * 
     * @return A ContentSecurityPolicy with frame-ancestors 'self'
     */
    public static ContentSecurityPolicy sameOriginPolicy() {
        final ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'self'");
        return csp;
    }

    /**
     * Returns a string representation of this CSP configuration for debugging purposes.
     * 
     * <p>
     * The format is: "ContentSecurityPolicy[frame-ancestors='...', directives=[...]]"
     * </p>
     * 
     * @return A string representation of this CSP configuration
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("ContentSecurityPolicy[");
        boolean hasContent = false;

        if (StringUtils.isNotEmpty(this.frameAncestors)) {
            sb.append("frame-ancestors='").append(this.frameAncestors).append("'");
            hasContent = true;
        }

        if (!this.additionalDirectives.isEmpty()) {
            if (hasContent) {
                sb.append(", ");
            }
            sb.append("directives=[");
            for (int i = 0; i < this.additionalDirectives.size(); i++) {
                if (i > 0) {
                    sb.append(", ");
                }
                sb.append("'").append(this.additionalDirectives.get(i)).append("'");
            }
            sb.append("]");
            hasContent = true;
        }

        if (!hasContent) {
            sb.append("empty");
        }

        sb.append("]");
        return sb.toString();
    }
}

