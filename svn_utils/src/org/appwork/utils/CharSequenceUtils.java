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

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;

/**
 * @author daniel
 * @date Dec 21, 2022
 *
 */
public class CharSequenceUtils {
    public static boolean contentEquals(final CharSequence x, final CharSequence y) {
        if (x == y) {
            return true;
        } else if (x == null && y != null) {
            return false;
        } else if (y == null && x != null) {
            return false;
        } else if (y.length() != x.length()) {
            return false;
        } else {
            if (x instanceof String) {
                return ((String) x).contentEquals(y);
            }
            for (int index = 0; index < y.length(); index++) {
                if (y.charAt(index) != x.charAt(index)) {
                    return false;
                }
            }
            return true;
        }
    }

    public final static boolean isEmpty(final CharSequence value) {
        return value == null || value.length() == 0 || isEmptyAfterTrim(value);
    }

    public final static boolean isNotEmpty(final CharSequence value) {
        return !isEmpty(value);
    }

    public static int indexOf(CharSequence source, final CharSequence target) {
        return indexOf(source, target, 0);
    }

    public static int indexOf(CharSequence source, final CharSequence target, int fromIndex) {
        if (source instanceof String && target instanceof String) {
            return ((String) source).indexOf((String) target, 0);
        } else {
            return indexOf(source, 0, target, 0, fromIndex);
        }
    }

    private static int indexOf(CharSequence source, int sourceOffset, CharSequence target, int targetOffset, int fromIndex) {
        // See java.lang.String.indexOf
        final int sourceCount = source.length();
        final int targetCount = target.length();
        if (fromIndex >= sourceCount) {
            return (targetCount == 0 ? sourceCount : -1);
        } else if (fromIndex < 0) {
            fromIndex = 0;
        }
        if (targetCount == 0) {
            return fromIndex;
        }
        char first = target.charAt(targetOffset);
        final int max = sourceOffset + (sourceCount - targetCount);
        for (int i = sourceOffset + fromIndex; i <= max; i++) {
            /* Look for first character. */
            if (source.charAt(i) != first) {
                while (++i <= max && source.charAt(i) != first) {
                    ;
                }
            }
            /* Found first character, now look at the rest of v2 */
            if (i <= max) {
                int j = i + 1;
                int end = j + targetCount - 1;
                for (int k = targetOffset + 1; j < end && source.charAt(j) == target.charAt(k); j++, k++) {
                    ;
                }
                if (j == end) {
                    /* Found whole string. */
                    return i - sourceOffset;
                }
            }
        }
        return -1;
    }

    public static CharSequence trim(CharSequence charSequence) {
        if (charSequence instanceof String) {
            return ((String) charSequence).trim();
        } else {
            // See java.lang.String.trim
            int len = charSequence.length();
            int st = 0;
            while (st < len && charSequence.charAt(st) <= ' ') {
                st++;
            }
            while (st < len && charSequence.charAt(len - 1) <= ' ') {
                len--;
            }
            return st > 0 || len < charSequence.length() ? charSequence.subSequence(st, len) : charSequence;
        }
    }

    public static boolean contains(CharSequence charSequence, final CharSequence search) {
        if (charSequence instanceof String && search instanceof String) {
            return ((String) charSequence).contains(search);
        } else {
            return indexOf(charSequence, search) != -1;
        }
    }

    public static boolean startsWith(CharSequence charSequence, final CharSequence prefix) {
        return startsWith(charSequence, prefix, 0);
    }

    public static boolean endsWith(CharSequence charSequence, CharSequence suffix) {
        return startsWith(charSequence, suffix, charSequence.length() - suffix.length());
    }

    public static byte[] getBytes(final CharSequence charSequence, final Charset cs) {
        if (cs == null) {
            throw new NullPointerException();
        } else if (charSequence instanceof String) {
            return ((String) charSequence).getBytes(cs);
        } else {
            final CharsetEncoder ce = cs.newEncoder().onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE);
            try {
                final CharBuffer cb = CharBuffer.wrap(charSequence);
                final ByteBuffer bb = ce.encode(cb);
                return bb.array();
            } catch (CharacterCodingException e) {
                throw new Error(e);
            }
        }
    }

    public final static boolean isEmptyAfterTrim(final CharSequence charSequence) {
        final int len = charSequence.length();
        if (len == 0) {
            return true;
        } else {
            int st = 0;
            while (st < len && charSequence.charAt(st) <= ' ') {
                st++;
            }
            return st == len;
        }
    }

    public static boolean startsWith(CharSequence charSequence, final CharSequence prefix, final int toffset) {
        if (charSequence instanceof String && prefix instanceof String) {
            return ((String) charSequence).startsWith((String) prefix, toffset);
        } else {
            // See java.lang.String.startsWith
            final int length = charSequence.length();
            int to = toffset;
            int po = 0;
            int pc = prefix.length();
            // Note: toffset might be near -1>>>1.
            if (toffset < 0 || toffset > length - pc) {
                return false;
            }
            while (--pc >= 0) {
                if (charSequence.charAt(to++) != prefix.charAt(po++)) {
                    return false;
                }
            }
            return true;
        }
    }
}
