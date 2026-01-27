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

import org.appwork.storage.StorableDoc;
import org.appwork.utils.StringUtils;

/**
 * Enum representing possible values for the Referrer-Policy HTTP header.
 *
 * The Referrer-Policy header controls how much referrer information should be included with requests made from a document.
 *
 * @author AppWork
 */
@StorableDoc("Enum representing possible values for the Referrer-Policy HTTP header. The Referrer-Policy header controls how much referrer information should be included with requests made from a document.")
public enum ReferrerPolicy {
    /**
     * No referrer information is sent.
     */
    @StorableDoc("No referrer information is sent. Header value: \"no-referrer\"")
    NO_REFERRER("no-referrer"),

    /**
     * No referrer information is sent when navigating from HTTPS to HTTP.
     */
    @StorableDoc("No referrer information is sent when navigating from HTTPS to HTTP. Header value: \"no-referrer-when-downgrade\"")
    NO_REFERRER_WHEN_DOWNGRADE("no-referrer-when-downgrade"),

    /**
     * Only the origin is sent as referrer.
     */
    @StorableDoc("Only the origin is sent as referrer. Header value: \"origin\"")
    ORIGIN("origin"),

    /**
     * Only the origin is sent as referrer when navigating from HTTPS to HTTP.
     */
    @StorableDoc("Only the origin is sent as referrer when navigating from HTTPS to HTTP. Header value: \"origin-when-cross-origin\"")
    ORIGIN_WHEN_CROSS_ORIGIN("origin-when-cross-origin"),

    /**
     * Send full referrer for same-origin requests, but only origin for cross-origin requests.
     */
    @StorableDoc("Send full referrer for same-origin requests, but only origin for cross-origin requests. Header value: \"same-origin\"")
    SAME_ORIGIN("same-origin"),

    /**
     * Send full referrer for same-origin requests, but no referrer for cross-origin requests.
     */
    @StorableDoc("Send full referrer for same-origin requests, but no referrer for cross-origin requests. Header value: \"strict-origin\"")
    STRICT_ORIGIN("strict-origin"),

    /**
     * Send full referrer for same-origin requests, but only origin for cross-origin requests and when downgrading.
     */
    @StorableDoc("Send full referrer for same-origin requests, but only origin for cross-origin requests and when downgrading. Header value: \"strict-origin-when-cross-origin\"")
    STRICT_ORIGIN_WHEN_CROSS_ORIGIN("strict-origin-when-cross-origin"),

    /**
     * Send full referrer information.
     */
    @StorableDoc("Send full referrer information. Header value: \"unsafe-url\"")
    UNSAFE_URL("unsafe-url");

    private final String value;

    private ReferrerPolicy(final String value) {
        this.value = value;
    }

    /**
     * Returns the header value as string.
     *
     * @return The header value (e.g., "no-referrer", "origin", "strict-origin-when-cross-origin")
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Converts a string to ReferrerPolicy (case-insensitive).
     *
     * @param value
     *            The header value string
     * @return The ReferrerPolicy enum value, or null if not found
     */
    public static ReferrerPolicy fromString(final String value) {
        if (value == null) {
            return null;
        }
        for (final ReferrerPolicy policy : values()) {
            if (policy.value.equalsIgnoreCase(value)) {
                return policy;
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
    public static ReferrerPolicy get(String value) {
        try {
            return valueOf(value);
        } catch (Exception e) {
            for (ReferrerPolicy v : values()) {
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
