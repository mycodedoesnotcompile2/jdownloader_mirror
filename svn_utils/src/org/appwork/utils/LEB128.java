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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author daniel
 * @date Aug 24, 2023
 *
 */
public class LEB128 {
    public static long read(final long maxShift, final InputStream is, final boolean alwaysThrowEOF) throws IOException {
        long ret = 0;
        int shift = 0;
        while (true) {
            final int read = is.read();
            if (read == -1) {
                if (shift > 0 && alwaysThrowEOF) {
                    throw new EOFException();
                } else {
                    return -1;
                }
            } else {
                final long value = read & 0x7f;
                final int msb = (read & 0xff) >> 7;
                ret |= (value << shift);
                if (msb == 0) {
                    return ret;
                } else {
                    shift += 7;
                    if (shift > maxShift) {
                        throw new ArithmeticException("number shift overflow:" + shift);
                    }
                }
            }
        }
    }

    public static long readLong(final InputStream is) throws IOException {
        return readLong(is, true);
    }

    public static long readLong(final InputStream is, final boolean alwaysThrowEOF) throws IOException {
        return read(56, is, alwaysThrowEOF);
    }

    public static int readInt(final InputStream is) throws IOException {
        return readInt(is, true);
    }

    public static int readInt(final InputStream is, final boolean alwaysThrowEOF) throws IOException {
        final long ret = read(28, is, alwaysThrowEOF);
        if (ret > Integer.MAX_VALUE) {
            throw new ArithmeticException("integer overflow");
        } else {
            return (int) ret;
        }
    }

    public static short readShort(final InputStream is) throws IOException {
        return readShort(is, true);
    }

    public static short readShort(final InputStream is, final boolean alwaysThrowEOF) throws IOException {
        final long ret = read(14, is, alwaysThrowEOF);
        if (ret > Short.MAX_VALUE) {
            throw new ArithmeticException("short overflow");
        } else {
            return (short) ret;
        }
    }

    public static byte readByte(final InputStream is) throws IOException {
        return readByte(is, true);
    }

    public static byte readByte(final InputStream is, final boolean alwaysThrowEOF) throws IOException {
        final long ret = read(7, is, alwaysThrowEOF);
        if (ret > Byte.MAX_VALUE) {
            throw new ArithmeticException("byte overflow");
        } else {
            return (byte) ret;
        }
    }

    public static void write(OutputStream out, long value) throws IOException {
        if (value < 0) {
            throw new IllegalArgumentException("value must be >= 0");
        } else {
            long remaining = value >>> 7;
            while (remaining != 0) {
                out.write((byte) ((value & 0x7f) | 0x80));
                value = remaining;
                remaining >>>= 7;
            }
            out.write((byte) (value & 0x7f));
        }
    }
}
