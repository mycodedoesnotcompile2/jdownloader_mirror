//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.nutils.encoding;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.HashSet;

import jd.parser.Regex;
import jd.parser.html.HTMLParser;

import org.appwork.utils.StringUtils;

public class Encoding {
    private final static char[] HEX = new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

    public static String base16Encode(final String input) {
        final byte[] byteArray = input.getBytes();
        StringBuffer hexBuffer = new StringBuffer(byteArray.length * 2);
        for (byte element : byteArray) {
            for (int j = 1; j >= 0; j--) {
                hexBuffer.append(Encoding.HEX[element >> j * 4 & 0xF]);
            }
        }
        return hexBuffer.toString();
    }

    public static byte[] base16Decode(String code) {
        while (code.length() % 2 > 0) {
            code += "0";
        }
        final byte[] res = new byte[code.length() / 2];
        int i = 0;
        while (i < code.length()) {
            res[i / 2] = (byte) Integer.parseInt(code.substring(i, i + 2), 16);
            i += 2;
        }
        return res;
    }

    public static String Base64Decode(final String base64) {
        if (base64 == null) {
            return null;
        } else {
            return Encoding.Base64Decode((CharSequence) base64).toString();
        }
    }

    public static CharSequence Base64Decode(final CharSequence base64) {
        if (base64 == null) {
            return null;
        }
        try {
            byte[] plain = org.appwork.utils.encoding.Base64.decode(base64);
            if (plain == null || plain.length == 0) {
                plain = org.appwork.utils.encoding.Base64.decodeFast(base64);
            }
            if (plain != null && plain.length > 0) {
                return new String(plain, "UTF-8");
            }
        } catch (final ArrayIndexOutOfBoundsException ignore) {
        } catch (final Exception e) {
            e.printStackTrace();
        }
        return base64;
    }

    public static String Base64Encode(final String plain) {
        if (plain == null) {
            return null;
        }
        // String base64 = new BASE64Encoder().encode(plain.getBytes());
        String base64;
        try {
            base64 = new String(org.appwork.utils.encoding.Base64.encodeToByte(plain.getBytes("UTF-8"), false));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            base64 = new String(org.appwork.utils.encoding.Base64.encodeToByte(plain.getBytes(), false));
        }
        return base64;
    }

    /**
     *
     * Wandelt HTML in CDATA um
     *
     * @param str
     * @return decoded string
     */
    public static String cdataEncode(String str) {
        if (str == null) {
            return null;
        }
        str = str.replaceAll("<", "&lt;");
        str = str.replaceAll(">", "&gt;");
        return str;
    }

    /**
     * Wendet htmlDecode an, bis es keine Änderungen mehr gibt. Aber max 50 mal!
     *
     * @param string
     * @return
     */
    public static String deepHtmlDecode(final String string) {
        String decoded, tmp;
        tmp = Encoding.htmlDecode(string);
        int i = 50;
        while (!tmp.equals(decoded = Encoding.htmlDecode(tmp))) {
            tmp = decoded;
            if (i-- <= 0) {
                System.err.println("Max Decodeingloop 50 reached!!!");
                return tmp;
            }
        }
        return tmp;
    }

    /**
     * Filtert alle Zeichen aus str die in filter nicht auftauchen
     *
     * @param str
     * @param filter
     * @return
     */
    public static String filterString(final String str, final String filter) {
        if (str == null || filter == null) {
            return "";
        }
        final byte[] org = str.getBytes();
        final byte[] mask = filter.getBytes();
        final byte[] ret = new byte[org.length];
        int count = 0;
        int i;
        for (i = 0; i < org.length; i++) {
            final byte letter = org[i];
            for (final byte element : mask) {
                if (letter == element) {
                    ret[count] = letter;
                    count++;
                    break;
                }
            }
        }
        return new String(ret).trim();
    }

    /**
     * DO NOT use for URLs!
     *
     * @param str
     * @return decoded string
     */
    public static String htmlDecode(String str) {
        if (str == null) {
            return null;
        } else {
            try {
                str = URLDecoder.decode(str, "UTF-8");
            } catch (final Throwable e) {
                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            }
            return Encoding.htmlOnlyDecode(str);
        }
    }

    public static String htmlOnlyDecode(String str) {
        return Encoding.htmlOnlyDecode(str, true);
    }

    public static String htmlOnlyDecode(String str, boolean trim) {
        if (str == null) {
            return null;
        } else {
            str = HTMLEntities.unhtmlentities(str, trim);
            str = HTMLEntities.unhtmlAmpersand(str);
            str = HTMLEntities.unhtmlAngleBrackets(str);
            str = HTMLEntities.unhtmlDoubleQuotes(str);
            str = HTMLEntities.unhtmlQuotes(str, trim);
            str = HTMLEntities.unhtmlSingleQuotes(str);
            return str;
        }
    }

    public static boolean isUrlCoded(final String str) {
        if (str == null) {
            return false;
        }
        try {
            if (URLDecoder.decode(str, "UTF-8").length() != str.length()) {
                return true;
            } else {
                return false;
            }
        } catch (final Exception e) {
            return false;
        }
    }

    public static boolean isHtmlEntityCoded(final String str) {
        if (str == null) {
            return false;
        }
        try {
            if (Encoding.htmlOnlyDecode(str).length() != str.length()) {
                return true;
            } else {
                return false;
            }
        } catch (final Exception e) {
            return false;
        }
    }

    public static String unicodeDecode(final String input) {
        return Encoding.unicodeDecode(input, false);
    }

    public static CharSequence unicodeDecode(final CharSequence input) {
        return Encoding.unicodeDecode(input, false);
    }

    public static String unicodeDecode(final String input, final boolean ignoreInvalidSequences) {
        final Object ret = Encoding.unicodeDecode((CharSequence) input, ignoreInvalidSequences);
        if (ret != null) {
            return ret.toString();
        } else {
            return null;
        }
    }

    /**
     * decodes unicode
     *
     * @param s
     * @return
     */
    public static CharSequence unicodeDecode(final CharSequence input, final boolean ignoreInvalidSequences) {
        if (input == null) {
            return null;
        }
        CharSequence s = input;
        if (true) {
            // convert any html based unicode as a pre correction
            final String regex = "(&#x([0-9a-f]{4});)";
            final String[] rmHtml = new Regex(s, regex).getColumn(0);
            if (rmHtml != null && rmHtml.length != 0) {
                String pre = s.toString();
                boolean replace = false;
                // lets prevent wasteful cycles
                final HashSet<String> dupe = new HashSet<String>();
                for (final String htmlrm : rmHtml) {
                    if (dupe.add(htmlrm) == true) {
                        final String[] rm = new Regex(htmlrm, regex).getRow(0);
                        if (rm[1] != null) {
                            pre = pre.replaceAll(rm[0], "\\\\u" + rm[1]);
                            replace = true;
                        }
                    }
                }
                if (replace) {
                    s = pre;
                }
            }
        }
        final StringBuilder sb = new StringBuilder();
        final StringBuilder decode = new StringBuilder();
        int ii;
        int i;
        loop: for (i = 0; i < s.length(); i++) {
            final char ch = s.charAt(i);
            // prevents StringIndexOutOfBoundsException with ending char equals case trigger
            if (s.length() != i + 1) {
                switch (ch) {
                case '%':
                case '\\':
                    final char escape = ch;
                    final char encoding = s.charAt(++i);
                    final int length;
                    switch (encoding) {
                    case 'u':
                        /* unicode encoding */
                        length = 4;
                        break;
                    case 'x':
                        /* hex encoding */
                        length = 2;
                        break;
                    default:
                        /* normal escaping */
                        sb.append(escape);
                        sb.append(encoding);
                        continue;
                    }
                    decode.setLength(0);
                    i++;
                    ii = i + length;
                    try {
                        for (; i < ii; i++) {
                            final char decode_ch = s.charAt(i);
                            if (!ignoreInvalidSequences) {
                                decode.append(decode_ch);
                            } else if (decode_ch >= '0' && decode_ch <= '9') {
                                decode.append(decode_ch);
                            } else if (decode_ch >= 'a' && decode_ch <= 'f') {
                                decode.append(decode_ch);
                            } else if (decode_ch >= 'A' && decode_ch <= 'F') {
                                decode.append(decode_ch);
                            } else {
                                sb.append(escape);
                                sb.append(encoding);
                                sb.append(decode);
                                i--;
                                continue loop;
                            }
                        }
                    } catch (StringIndexOutOfBoundsException e) {
                        if (ignoreInvalidSequences) {
                            sb.append(escape);
                            sb.append(encoding);
                            sb.append(decode);
                            return sb;
                        } else {
                            throw e;
                        }
                    }
                    i--;
                    sb.append((char) Encoding.parseLong(decode));
                    continue;
                }
            }
            sb.append(ch);
        }
        return sb;
    }

    // Taken from Long.parseLong, modified to have CharSequence parameter with fixed radix 16
    private static long parseLong(final CharSequence s) throws NumberFormatException {
        final int radix = 16;
        long result = 0;
        int i = 0, len = s.length();
        final long limit = -Long.MAX_VALUE;
        int digit;
        if (len > 0) {
            final long multmin = limit / radix;
            while (i < len) {
                digit = Character.digit(s.charAt(i++), radix);
                if (digit < 0) {
                    throw new NumberFormatException("For input string: \"" + s + "\"");
                } else if (result < multmin) {
                    throw new NumberFormatException("For input string: \"" + s + "\"");
                } else {
                    result *= radix;
                    if (result < limit + digit) {
                        throw new NumberFormatException("For input string: \"" + s + "\"");
                    } else {
                        result -= digit;
                    }
                }
            }
        } else {
            throw new NumberFormatException("For input string: \"" + s + "\"");
        }
        // Accumulating negatively avoids surprises near MAX_VALUE
        return -result;
    }

    @Deprecated
    public static String urlDecode(String urlcoded, final boolean isUrl) {
        if (urlcoded == null) {
            return null;
        }
        if (isUrl) {
            final boolean seemsValidURL = urlcoded.startsWith("http://") || urlcoded.startsWith("https://");
            if (seemsValidURL == false) {
                urlcoded = urlcoded.replaceAll("%2F", "/");
                urlcoded = urlcoded.replaceAll("%3A", ":");
                urlcoded = urlcoded.replaceAll("%3F", "?");
                urlcoded = urlcoded.replaceAll("%3D", "=");
                urlcoded = urlcoded.replaceAll("%26", "&");
                urlcoded = urlcoded.replaceAll("%23", "#");
            }
            final boolean seemsFileURL = StringUtils.startsWithCaseInsensitive(urlcoded, HTMLParser.protocolFile);
            if (seemsFileURL) {
                urlcoded = urlcoded.replaceAll("%20", " ");
            }
        } else {
            try {
                urlcoded = URLDecoder.decode(urlcoded, "UTF-8");
            } catch (final Exception e) {
                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            }
        }
        return urlcoded;
    }

    /**
     * WARNING: this method is ONLY for application/x-www-form-urlencoded !!!!
     *
     * Always safe: A-Z a-z 0-9 - . _ ~ ( ) ' ! * : @ , ;
     *
     * Sometimes Safe: Paths + & =
     *
     * Sometimes Safe: Queries: ? /
     *
     * Sometimes Safe: Fragments: ? / # + & =
     *
     * Never Safe: (RFC 3986) <space> <control-characters> <extended-ascii> <unicode> % < > [ ] { } | \ ^
     **/
    public static String urlEncode(final String str) {
        if (str != null) {
            try {
                return URLEncoder.encode(str, "UTF-8");
            } catch (final Exception e) {
                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            }
        }
        return str;
    }

    @Deprecated
    public static String urlEncode_light(final String url) {
        return (String) Encoding.urlEncodeCharSequence_light(url);
    }

    @Deprecated
    public static CharSequence urlEncodeCharSequence_light(final CharSequence url) {
        if (url == null) {
            return null;
        }
        boolean urlEncodeLight = false;
        for (int i = 0; i < url.length(); i++) {
            final char ch = url.charAt(i);
            if (ch == ' ') {
                urlEncodeLight = true;
                break;
            } else if (ch >= 33 && ch <= 38) {
            } else if (ch >= 40 && ch <= 59) {
            } else if (ch == 61) {
            } else if (ch >= 63 && ch <= 95) {
            } else if (ch >= 97 && ch <= 126) {
            } else {
                urlEncodeLight = true;
                break;
            }
        }
        if (urlEncodeLight) {
            final StringBuffer sb = new StringBuffer();
            for (int i = 0; i < url.length(); i++) {
                final char ch = url.charAt(i);
                if (ch == ' ') {
                    sb.append("%20");
                } else if (ch >= 33 && ch <= 38) {
                    sb.append(ch);
                } else if (ch >= 40 && ch <= 59) {
                    sb.append(ch);
                } else if (ch == 61) {
                    sb.append(ch);
                } else if (ch >= 63 && ch <= 95) {
                    sb.append(ch);
                } else if (ch >= 97 && ch <= 126) {
                    sb.append(ch);
                } else {
                    try {
                        sb.append(URLEncoder.encode(String.valueOf(ch), "UTF-8"));
                    } catch (final Exception e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                        break;
                    }
                }
            }
            return sb.toString();
        }
        return url;
    }

    /**
     * @author JD-Team
     * @param str
     * @return str als UTF8Decodiert
     */
    public static String UTF8Decode(final String str) {
        return Encoding.UTF8Decode(str, null);
    }

    public static String UTF8Decode(final String str, final String sourceEncoding) {
        if (str == null) {
            return null;
        }
        try {
            if (sourceEncoding != null) {
                return new String(str.getBytes(sourceEncoding), "UTF-8");
            } else {
                return new String(str.getBytes(), "UTF-8");
            }
        } catch (final UnsupportedEncodingException e) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            return str;
        }
    }

    /**
     * @author JD-Team
     * @param str
     * @return str als UTF8 Kodiert
     */
    public static String UTF8Encode(final String str) {
        try {
            return new String(str.getBytes("UTF-8"));
        } catch (final Exception e) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            return null;
        }
    }

    public static String atbashDecode(String crypted) {
        if (crypted == null) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        for (int index = 0; index < crypted.length(); index++) {
            char ch = crypted.charAt(index);
            if (ch >= 'a' && ch <= 'z') {
                sb.append((char) ('z' - (ch - 'a')));
            } else if (ch >= 'A' && ch <= 'Z') {
                sb.append((char) ('Z' - (ch - 'A')));
            } else if (ch >= '0' && ch <= '9') {
                sb.append(ch);
            } else if (ch == ':') {
                sb.append(':');
            } else if (ch == '$') {
                sb.append('/');
            } else if (ch == '+') {
                sb.append('.');
            } else {
                sb.append(ch);
            }
        }
        return sb.toString();
    }

    public static String atbashEncode(String crypted) {
        if (crypted == null) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        for (int index = 0; index < crypted.length(); index++) {
            char ch = crypted.charAt(index);
            if (ch >= 'a' && ch <= 'z') {
                sb.append((char) ('a' - (ch - 'z')));
            } else if (ch >= 'A' && ch <= 'Z') {
                sb.append((char) ('A' - (ch - 'Z')));
            } else if (ch >= '0' && ch <= '9') {
                sb.append(ch);
            } else if (ch == ':') {
                sb.append(':');
            } else if (ch == '/') {
                sb.append('$');
            } else if (ch == '.') {
                sb.append('+');
            } else {
                sb.append(ch);
            }
        }
        return sb.toString();
    }
}
