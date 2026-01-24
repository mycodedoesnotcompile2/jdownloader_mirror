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
import org.appwork.utils.StringUtils;

/**
 * Enum representing possible values for the X-Frame-Options HTTP header.
 *
 * The X-Frame-Options header protects against clickjacking attacks by controlling whether a page can be displayed in a frame, iframe, embed
 * or object.
 *
 * @author AppWork
 */
@StorableDoc("Enum representing possible values for the X-Frame-Options HTTP header. The X-Frame-Options header protects against clickjacking attacks by controlling whether a page can be displayed in a frame, iframe, embed or object.")
public enum XFrameOptions {
    /**
     * The page cannot be displayed in a frame, regardless of the site attempting to do so.
     */
    @StorableDoc("The page cannot be displayed in a frame, regardless of the site attempting to do so. Header value: \"DENY\"")
    DENY("DENY"),

    /**
     * The page can only be displayed in a frame on the same origin as the page itself.
     */
    @StorableDoc("The page can only be displayed in a frame on the same origin as the page itself. Header value: \"SAMEORIGIN\"")
    SAMEORIGIN("SAMEORIGIN"),

    /**
     * The page can be displayed in a frame only on the specified origin. Note: This value is deprecated and not widely supported. Use
     * Content-Security-Policy frame-ancestors instead.
     */
    @StorableDoc("The page can be displayed in a frame only on the specified origin. Note: This value is deprecated and not widely supported. Use Content-Security-Policy frame-ancestors instead. Header value: \"ALLOW-FROM <uri>\"")
    ALLOW_FROM("ALLOW-FROM");

    private final String value;

    private XFrameOptions(final String value) {
        this.value = value;
    }

    /**
     * Returns the header value as string.
     *
     * @return The header value (e.g., "DENY", "SAMEORIGIN", "ALLOW-FROM")
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Converts a string to XFrameOptions (case-insensitive).
     *
     * @param value
     *            The header value string
     * @return The XFrameOptions enum value, or null if not found
     */
    public static XFrameOptions fromString(final String value) {
        if (value == null) {
            return null;
        }
        final String upperValue = value.toUpperCase();
        for (final XFrameOptions option : values()) {
            if (option.value.equalsIgnoreCase(upperValue) || upperValue.startsWith(option.value)) {
                return option;
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

    /**
     * @param value2
     * @return
     */
    public static XFrameOptions get(String value) {
        try {
            return valueOf(value);
        } catch (Exception e) {
            for (XFrameOptions v : values()) {
                if (StringUtils.equalsIgnoreCase(v.getValue(), value)) {
                    return v;
                }
                if (StringUtils.equalsIgnoreCase(v.name(), value)) {
                    return v;
                }
            }
        }
        return null;
    }
}
