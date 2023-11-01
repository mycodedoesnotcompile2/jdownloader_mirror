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
package org.appwork.utils.encoding;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;

/**
 * @author daniel
 *
 */
/* https://en.wikipedia.org/wiki/URL_encoding */
public class URLEncode {
    /* Unreserved Characters */
    /* https://datatracker.ietf.org/doc/html/rfc2396 */
    public static final String RFC2396_RFC_UNRESERVEDCHARACTERS  = "0123456789" + "abcdefghijklmnopqrstuvwxyz" + "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "-_.!~*'()";
    /**
     * taken from URLEncoder class
     *
     * It appears that both Netscape and Internet Explorer escape all special characters from this list with the exception of "-", "_", ".",
     * "*". While it is not clear why they are escaping the other characters, perhaps it is safest to assume that there might be contexts in
     * which the others are unsafe if not escaped. Therefore, we will use the same list. It is also noteworthy that this is consistent with
     * O'Reilly's "HTML: The Definitive Guide" (page 164).
     */
    public static final String RFC2396_JAVA_UNRESERVEDCHARACTERS = "0123456789" + "abcdefghijklmnopqrstuvwxyz" + "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "-_.*";
    /* https://datatracker.ietf.org/doc/html/rfc3986, obsoletes RFC2396 */
    public static final String RFC3986_RFC_UNRESERVEDCHARACTERS  = "0123456789" + "abcdefghijklmnopqrstuvwxyz" + "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "-._~";

    public static interface Decoder {
        public String decode(String value) throws UnsupportedEncodingException;
    }

    public static interface Encoder {
        public String encode(String value) throws UnsupportedEncodingException;
    }

    public static String encodeRFC2396(final String input) {
        return encodeRFC2396((CharSequence) input);
    }

    /* http://www.ietf.org/rfc/rfc2396.txt */
    public static String encodeRFC2396(final CharSequence input) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < input.length(); i++) {
            final char ch = input.charAt(i);
            if (ch == ' ') {
                sb.append("+");
            } else if (URLEncode.RFC2396_JAVA_UNRESERVEDCHARACTERS.indexOf(ch) != -1) {
                sb.append(ch);
            } else {
                if (ch > 255) {
                    /**
                     * 2.1 URI and non-ASCII characters
                     *
                     * For original character sequences that contain non-ASCII characters, however, the situation is more difficult.
                     * Internet protocols that transmit octet sequences intended to represent character sequences are expected to provide
                     * some way of identifying the charset used, if there might be more than one [RFC2277]. However, there is currently no
                     * provision within the generic URI syntax to accomplish this identification. An individual URI scheme may require a
                     * single charset, define a default charset, or provide a way to indicate the charset used.
                     */
                    /* not allowed, replaced by + */
                    sb.append("+");
                } else {
                    /* hex formatted */
                    try {
                        sb.append(URLEncoder.encode("" + ch, "UTF-8"));
                    } catch (UnsupportedEncodingException ignore) {
                    }
                }
            }
        }
        return sb.toString();
    }

    public static String decodeURIComponent(final String input) {
        try {
            return decodeURIComponent(input, "UTF-8", true);
        } catch (UnsupportedEncodingException ignore) {
        } catch (IllegalArgumentException ignore) {
        }
        return null;
    }

    public static String decodeURIComponent(final String input, final String charSet, final boolean ignoreDecodeError) throws UnsupportedEncodingException, IllegalArgumentException {
        return decodeURIComponent(input, new Decoder() {

            @Override
            public String decode(final String value) throws UnsupportedEncodingException {
                try {
                    return URLDecoder.decode(value, charSet);
                } catch (IllegalArgumentException e) {
                    if (ignoreDecodeError) {
                        return value;
                    } else {
                        throw e;
                    }
                }
            }
        });
    }

    public static String decodeURIComponent(final CharSequence input, final Decoder decoder) throws UnsupportedEncodingException, IllegalArgumentException {
        if (input == null) {
            return null;
        } else {
            final StringBuilder ret = new StringBuilder();
            final StringBuilder sequence = new StringBuilder("");
            int sequenceStep = 0; // 0=%, 1= first hex, 2 = second hex
            for (int i = 0; i < input.length(); i++) {
                final char nextChar = input.charAt(i);
                if (nextChar == '%') {
                    if (sequenceStep == 1 || sequenceStep == 2) {
                        // finalize incomplete sequence,first or second hex character missing
                        finalizeDecoderSequence(decoder, sequence, ret);
                    }
                    sequence.append(nextChar);
                    sequenceStep = 1;
                } else {
                    if ((sequenceStep == 1 || sequenceStep == 2) && ((nextChar >= '0' && nextChar <= '9') || (nextChar >= 'a' && nextChar <= 'f') || (nextChar >= 'A' && nextChar <= 'F'))) {
                        // valid first or second hex character
                        sequence.append(nextChar);
                        sequenceStep++;
                    } else {
                        // finalize incomplete sequence
                        finalizeDecoderSequence(decoder, sequence, ret);
                        ret.append(nextChar);
                        sequenceStep = 0;
                    }
                }
            }
            finalizeDecoderSequence(decoder, sequence, ret);
            return ret.toString();
        }
    }

    private static void finalizeDecoderSequence(final Decoder decoder, final StringBuilder decode, final StringBuilder output) throws UnsupportedEncodingException, IllegalArgumentException {
        if (decode.length() / 3 > 0) {
            // finalize completed sequences
            final int completedSequenceLength = (decode.length() / 3) * 3;
            final String completedSequence = decode.substring(0, completedSequenceLength);
            decode.delete(0, completedSequenceLength);
            output.append(decoder.decode(completedSequence));
        }
        if (decode.length() > 0) {
            output.append(decode);
            decode.delete(0, decode.length());
        }
    }

    private static boolean encodeURLSegment(final StringBuilder buf, final StringBuilder output, final Encoder encoder) throws UnsupportedEncodingException {
        if (buf.length() > 0) {
            output.append(encoder.encode(buf.toString()));
            buf.delete(0, buf.length());
            return true;
        } else {
            return false;
        }
    }

    public static String encodeURIComponent(final CharSequence input) {
        if (input == null) {
            return null;
        } else {
            return encodeURIComponent(input, new Encoder() {

                @Override
                public String encode(String value) throws UnsupportedEncodingException {
                    return URLEncoder.encode(value, "UTF-8");
                }
            });
        }
    }

    public static String encodeURIComponent(final CharSequence input, final Encoder encoder) {
        if (input == null) {
            return null;
        } else {
            try {
                final StringBuilder output = new StringBuilder();
                final StringBuilder encodeBuffer = new StringBuilder();
                for (int i = 0; i < input.length(); i++) {
                    final char ch = input.charAt(i);
                    if (ch == ' ') {
                        encodeURLSegment(encodeBuffer, output, encoder);
                        output.append("%20");
                    } else if (URLEncode.RFC3986_RFC_UNRESERVEDCHARACTERS.indexOf(ch) != -1) {
                        encodeURLSegment(encodeBuffer, output, encoder);
                        output.append(ch);
                    } else {
                        encodeBuffer.append(ch);
                    }
                }
                encodeURLSegment(encodeBuffer, output, encoder);
                return output.toString();
            } catch (UnsupportedEncodingException ignore) {
                throw new RuntimeException(ignore);
            }
        }
    }

    public static String encodeURIComponent(final String input) {
        return encodeURIComponent((CharSequence) input);
    }

}
