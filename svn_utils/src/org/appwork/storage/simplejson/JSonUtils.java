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
package org.appwork.storage.simplejson;

import java.util.Locale;

import org.appwork.utils.formatter.HexFormatter;

/**
 * @author thomas
 *
 */
public class JSonUtils {
    public static String escape(final String s) {
        return escape(s, true);
    }

    public static String escape(final String s, boolean doubleQuoteEscape) {
        final StringBuilder sb = new StringBuilder();
        char ch;
        String ss;
        for (int i = 0; i < s.length(); i++) {
            ch = s.charAt(i);
            switch (ch) {
            case '"':
                if (!doubleQuoteEscape) {
                    break;
                }
                sb.append("\\\"");
                continue;
            case '\'':
                if (doubleQuoteEscape) {
                    break;
                }
                sb.append("\\'");
                continue;
            case '\\':
                sb.append("\\\\");
                continue;
            case '\b':
                sb.append("\\b");
                continue;
            case '\f':
                sb.append("\\f");
                continue;
            case '\n':
                sb.append("\\n");
                continue;
            case '\r':
                sb.append("\\r");
                continue;
            case '\t':
                sb.append("\\t");
                continue;
            }
            // '\u0000' && ch <= '\u001F' are controll characters )(
            // http://www.ietf.org/rfc/rfc4627.txt 5.2 Strings)
            // the text says U+0000 >>> to U+001F but the syntax diagram just
            // says control character, which in >>> Unicode 6.3 also includes
            // U+007F to U+009F
            // http://www.unicode.org/charts/PDF/U2000.pdf
            if (ch >= '\u0000' && ch <= '\u001F' || ch >= '\u007F' && ch <= '\u009F' || ch >= '\u2000' && ch <= '\u20FF') {
                ss = Integer.toHexString(ch);
                sb.append("\\u");
                for (int k = 0; k < 4 - ss.length(); k++) {
                    sb.append('0');
                }
                sb.append(ss.toUpperCase(Locale.ENGLISH));
                continue;
            }
            sb.append(ch);
        }
        return sb.toString();
    }

    public static void main(final String[] args) {
        final String str = "\\\r\t\b\n\f\"abc\u2011\u0026\uFFFF";
        for (int i = 0; i < str.length(); i++) {
            final String s = str.substring(i, i + 1);
            final String str2 = JSonUtils.escape(s);
            final String str3 = JSonUtils.unescape(str2);
            System.out.println(str3);
            System.out.println(s + " - " + HexFormatter.byteArrayToHex(s.getBytes()));
            System.out.println("OK: " + s + "|" + str3.equals(s));
        }
        final String s = str;
        final String str2 = JSonUtils.escape(s);
        final String str3 = JSonUtils.unescape(str2);
        System.out.println(HexFormatter.byteArrayToHex(str3.getBytes()));
        System.out.println(HexFormatter.byteArrayToHex(s.getBytes()));
        System.out.println("OK: |" + str3.equals(s));
        System.out.println("Max Value: " + (0xffff == JSonUtils.unescape("\\uFFFF").charAt(0)));
        // System.out.println(str3);
    }

    /**
     * @param string
     * @return
     */
    public static String unescape(final String s) {
        char ch;
        final StringBuilder sb = new StringBuilder();
        final StringBuilder sb2 = new StringBuilder();
        int ii;
        int i;
        for (i = 0; i < s.length(); i++) {
            ch = s.charAt(i);
            switch (ch) {
            case '\\':
                ch = s.charAt(++i);
                switch (ch) {
                case '"':
                    sb.append('"');
                    continue;
                case '\\':
                    sb.append('\\');
                    continue;
                case 'r':
                    sb.append('\r');
                    continue;
                case 'n':
                    sb.append('\n');
                    continue;
                case 't':
                    sb.append('\t');
                    continue;
                case 'f':
                    sb.append('\f');
                    continue;
                case 'b':
                    sb.append('\b');
                    continue;
                case 'u':
                    sb2.delete(0, sb2.length());
                    i++;
                    ii = i + 4;
                    for (; i < ii; i++) {
                        ch = s.charAt(i);
                        if (sb2.length() > 0 || ch != '0') {
                            sb2.append(ch);
                        }
                    }
                    i--;
                    sb.append((char) Integer.parseInt(sb2.toString(), 16));
                    continue;
                default:
                    sb.append(ch);
                    continue;
                }
            }
            sb.append(ch);
        }
        return sb.toString();
    }
}
