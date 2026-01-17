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

import org.appwork.storage.StorableDoc;

/**
 * Enum representing possible values for the Sec-Fetch-Site HTTP header.
 * 
 * The Sec-Fetch-Site header indicates the relationship between the origin of the resource
 * and the origin of the request.
 * 
 * @author AppWork
 */
@StorableDoc("Enum representing possible values for the Sec-Fetch-Site HTTP header. The Sec-Fetch-Site header indicates the relationship between the origin of the resource and the origin of the request.")
public enum SecFetchSite {
    /**
     * The request is from a different site (different scheme, domain, or port).
     */
    @StorableDoc("The request is from a different site (different scheme, domain, or port). Header value: \"cross-site\"")
    CROSS_SITE("cross-site"),
    
    /**
     * The request is from the same site but different origin (same scheme and domain, different port).
     */
    @StorableDoc("The request is from the same site but different origin (same scheme and domain, different port). Header value: \"same-site\"")
    SAME_SITE("same-site"),
    
    /**
     * The request is from the same origin.
     */
    @StorableDoc("The request is from the same origin. Header value: \"same-origin\"")
    SAME_ORIGIN("same-origin"),
    
    /**
     * The request is initiated by the user (e.g., typing URL in address bar, bookmark, etc.).
     */
    @StorableDoc("The request is initiated by the user (e.g., typing URL in address bar, bookmark, etc.). Header value: \"none\"")
    NONE("none");

    private final String value;

    private SecFetchSite(final String value) {
        this.value = value;
    }

    /**
     * Returns the header value as string.
     * 
     * @return The header value (e.g., "cross-site", "same-origin", "none")
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Converts a string to SecFetchSite (case-insensitive).
     * 
     * @param value
     *            The header value string
     * @return The SecFetchSite enum value, or null if not found
     */
    public static SecFetchSite fromString(final String value) {
        if (value == null) {
            return null;
        }
        for (final SecFetchSite site : values()) {
            if (site.value.equalsIgnoreCase(value)) {
                return site;
            }
        }
        return null;
    }

    /**
     * Returns the header value as string.
     * 
     * @return The header value
     */
    @Override
    public String toString() {
        return this.value;
    }
}

