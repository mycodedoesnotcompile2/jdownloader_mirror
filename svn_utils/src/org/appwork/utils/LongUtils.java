/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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

/**
 * @author Daniel Wilhelm
 * @date 13.11.2026
 *
 */
public class LongUtils {

    public static long pack(final int high, final int low) {
        return (((long) high) << 32) | (low & 0xFFFFFFFFL);
    }

    public static int getHighInt(final long packed) {
        return (int) (packed >> 32);
    }

    public static int getLowInt(final long packed) {
        return (int) packed;
    }

    public static long pack(final float high, final float low) {
        final int highBits = Float.floatToRawIntBits(high);
        final int lowBits = Float.floatToRawIntBits(low);
        return (((long) highBits) << 32) | (lowBits & 0xFFFFFFFFL);
    }

    public static float getHighFloat(final long packed) {
        return Float.intBitsToFloat((int) (packed >> 32));
    }

    public static float getLowFloat(final long packed) {
        return Float.intBitsToFloat((int) packed);
    }

    public static long packBytes(final byte[] bytes) {
        if (bytes == null || bytes.length != 8) {
            throw new IllegalArgumentException("Array muss genau 8 Bytes lang sein.");
        }
        long result = 0;
        for (int i = 0; i < 8; i++) {
            result = (result << 8) | (bytes[i] & 0xFFL);
        }
        return result;
    }

    public static byte[] unpackBytes(long packed) {
        final byte[] result = new byte[8];
        for (int i = 7; i >= 0; i--) {
            result[i] = (byte) (packed & 0xFFL);
            packed >>= 8;
        }
        return result;
    }

    private static void checkByteIndex(final int index) {
        if (index < 0 || index > 7) {
            throw new IndexOutOfBoundsException("Index muss zwischen 0 und 7 liegen: " + index);
        }
    }

    public static byte getByte(final long packed, final int index) {
        checkByteIndex(index);
        final int shift = (7 - index) * 8;
        return (byte) ((packed >> shift) & 0xFFL);
    }

    public static long setByte(final long packed, final int index, final byte value) {
        checkByteIndex(index);
        final int shift = (7 - index) * 8;
        final long mask = ~(0xFFL << shift); // Löscht die 8 Bits an der Zielposition
        return (packed & mask) | ((value & 0xFFL) << shift);
    }

    public static long packShorts(final short s0, final short s1, final short s2, final short s3) {
        return ((s0 & 0xFFFFL) << 48) | ((s1 & 0xFFFFL) << 32) | ((s2 & 0xFFFFL) << 16) | (s3 & 0xFFFFL);
    }

    public static long packShorts(final short[] shorts) {
        if (shorts == null || shorts.length != 4) {
            throw new IllegalArgumentException("Array muss genau 4 Shorts lang sein.");
        }
        return packShorts(shorts[0], shorts[1], shorts[2], shorts[3]);
    }

    public static short[] unpackShorts(final long packed) {
        return new short[] { (short) (packed >> 48), (short) (packed >> 32), (short) (packed >> 16), (short) packed };
    }

    public static short getShort(final long packed, final int index) {
        checkShortIndex(index);
        final int shift = (3 - index) * 16;
        return (short) ((packed >> shift) & 0xFFFFL);
    }

    public static long setShort(final long packed, final int index, final short value) {
        checkShortIndex(index);
        final int shift = (3 - index) * 16;
        final long mask = ~(0xFFFFL << shift);
        return (packed & mask) | ((value & 0xFFFFL) << shift);
    }

    private static void checkShortIndex(int index) {
        if (index < 0 || index > 3) {
            throw new IndexOutOfBoundsException("Index muss zwischen 0 und 3 liegen: " + index);
        }
    }

    public static long packChars(final char c0, final char c1, final char c2, final char c3) {
        return ((c0 & 0xFFFFL) << 48) | ((c1 & 0xFFFFL) << 32) | ((c2 & 0xFFFFL) << 16) | (c3 & 0xFFFFL);
    }

    public static long packChars(final char[] chars) {
        if (chars == null || chars.length != 4) {
            throw new IllegalArgumentException("Array muss genau 4 Chars lang sein.");
        }
        return packChars(chars[0], chars[1], chars[2], chars[3]);
    }

    public static char[] unpackChars(final long packed) {
        return new char[] { (char) (packed >> 48), (char) (packed >> 32), (char) (packed >> 16), (char) packed };
    }

    public static char getChar(final long packed, final int index) {
        checkCharIndex(index);
        final int shift = (3 - index) * 16;
        return (char) ((packed >> shift) & 0xFFFFL);
    }

    public static long setChar(final long packed, final int index, final char value) {
        checkCharIndex(index);
        final int shift = (3 - index) * 16;
        final long mask = ~(0xFFFFL << shift);
        return (packed & mask) | ((value & 0xFFFFL) << shift);
    }

    private static void checkCharIndex(final int index) {
        if (index < 0 || index > 3) {
            throw new IndexOutOfBoundsException("Index muss zwischen 0 und 3 liegen: " + index);
        }
    }

}
