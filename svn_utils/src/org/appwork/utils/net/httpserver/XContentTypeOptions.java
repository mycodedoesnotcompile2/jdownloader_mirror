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
 * Enum representing possible values for the X-Content-Type-Options HTTP header.
 * 
 * The X-Content-Type-Options header prevents Google Chrome and Internet Explorer from
 * trying to mime-sniff the content-type of a response away from the one being declared
 * by the server. It reduces exposure to drive-by downloads and the risks of user uploaded
 * content that, with clever naming, could be treated as a different content-type.
 * 
 * @author AppWork
 */
@StorableDoc("Enum representing possible values for the X-Content-Type-Options HTTP header. The X-Content-Type-Options header prevents Google Chrome and Internet Explorer from trying to mime-sniff the content-type of a response away from the one being declared by the server.")
public enum XContentTypeOptions {
    /**
     * Prevents MIME type sniffing. This is the only valid value for this header.
     */
    @StorableDoc("Prevents MIME type sniffing. This is the only valid value for this header. Header value: \"nosniff\"")
    NOSNIFF("nosniff");

    private final String value;

    private XContentTypeOptions(final String value) {
        this.value = value;
    }

    /**
     * Returns the header value as string.
     * 
     * @return The header value ("nosniff")
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Converts a string to XContentTypeOptions (case-insensitive).
     * 
     * @param value
     *            The header value string
     * @return The XContentTypeOptions enum value, or null if not found
     */
    public static XContentTypeOptions fromString(final String value) {
        if (value == null) {
            return null;
        }
        for (final XContentTypeOptions option : values()) {
            if (option.value.equalsIgnoreCase(value)) {
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
}

