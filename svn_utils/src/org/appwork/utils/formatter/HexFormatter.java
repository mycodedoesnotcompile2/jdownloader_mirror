/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.utils.formatter;

import java.nio.ByteBuffer;

public class HexFormatter {
    /**
     * Converts a Bytearray to a HexString
     *
     * @param digest
     * @return
     */

    public static String byteArrayToHex(final byte[] digest) {
        return toHex(digest, new StringBuilder()).toString();
    }

    public static StringBuilder toHex(final byte[] input, final StringBuilder output) {
        output.ensureCapacity(output.capacity() + input.length * 2);
        for (final byte d : input) {
            final String tmp = Integer.toHexString(d & 0xFF);
            if (tmp.length() < 2) {
                output.append('0');
            }
            output.append(tmp);
        }
        return output;
    }

    public static StringBuilder toHex(final ByteBuffer digest, final StringBuilder output) {
        output.ensureCapacity(output.capacity() + digest.remaining() * 2);
        while (digest.hasRemaining()) {
            final byte b = digest.get();
            final String tmp = Integer.toHexString(b & 0xFF);
            if (tmp.length() < 2) {
                output.append('0');
            }
            output.append(tmp);
        }
        return output;
    }

    public static String byteBufferToHex(final ByteBuffer digest) {
        return toHex(digest, new StringBuilder()).toString();
    }

    public static byte[] hexToByteArray(final String s) {
        if (s == null) {
            return null;
        }
        final int len = s.length();
        final byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16));
        }
        return data;
    }
}
