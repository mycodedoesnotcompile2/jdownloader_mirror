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
package org.appwork.utils.encoding;

import java.io.IOException;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;

/**
 * @author daniel
 * @date Mar 24, 2022
 *
 *       https://datatracker.ietf.org/doc/html/rfc2047
 *
 *       https://www.rfc-editor.org/info/rfc2047
 */
public class RFC2047 {

    public static enum Encoding {
        B {

            @Override
            public CharSequence decode(CharSequence encoded, Charset charSet) throws IOException {
                final String decoded = new String(Base64.decode(encoded), charSet);
                final String out = URLDecoder.decode(decoded, charSet.displayName());
                return out;
            }

        },
        Q {

            @Override
            public CharSequence decode(CharSequence encoded, Charset charSet) throws IOException {
                final StringBuffer sb = new StringBuffer();
                final Matcher matcher = Pattern.compile("(=[a-fA-F0-9]{2})+").matcher(encoded);
                while (matcher.find()) {
                    final int start = matcher.start();
                    final int end = matcher.end();
                    final CharSequence match = encoded.subSequence(start, end);
                    matcher.appendReplacement(sb, Matcher.quoteReplacement(URLDecoder.decode(match.toString().replace("=", "%"), charSet.name())));
                }
                if (sb.length() == 0) {
                    return encoded;
                } else {
                    matcher.appendTail(sb);
                    return sb;
                }
            }

        };

        public abstract CharSequence decode(CharSequence encoded, Charset charSet) throws IOException;
    }

    public static abstract class Result {
        public abstract Encoding getEncoding();

        public abstract CharSequence getInput();

        public abstract CharSequence getOutput();

        public abstract CharSequence getEncoded();

        public abstract Charset getCharset();

        @Override
        public String toString() {
            return "Encoding:" + getEncoding() + "|Charset:" + getCharset() + "|Output:" + getOutput();
        }
    }

    protected static final String PATTERN_RELAXED = "(=(\\?|_)(.*?)\\2(B|b|Q|q)\\2(.+?)\\2=)";
    protected static final String PATTERN_STRICT  = "(=(\\?)(.*?)\\2(B|b|Q|q)\\2(.+?)\\2=)";

    protected Matcher getMatcher(final CharSequence in, final boolean relaxed) {
        final Matcher matcher;
        if (relaxed) {
            matcher = Pattern.compile(PATTERN_RELAXED).matcher(in);
        } else {
            matcher = Pattern.compile(PATTERN_STRICT).matcher(in);
        }
        return matcher;
    }

    public CharSequence decode(final CharSequence in) {
        try {
            return decode(in, false, false);
        } catch (IOException e) {
            return in;
        }
    }

    public CharSequence decode(final CharSequence in, final boolean relaxed) throws IOException {
        return decode(in, relaxed, false);
    }

    public CharSequence decode(final CharSequence in, final boolean relaxed, final boolean throwException) throws IOException {
        final Matcher matcher = getMatcher(in, relaxed);
        final StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            final int start = matcher.start();
            final int end = matcher.end();
            final CharSequence match = in.subSequence(start, end);
            final Result next = decodeSingle(match, relaxed, throwException);
            if (next != null) {
                matcher.appendReplacement(sb, Matcher.quoteReplacement(next.getOutput().toString()));
            }
        }
        if (sb.length() == 0) {
            return in;
        } else {
            matcher.appendTail(sb);
            return sb;
        }
    }

    protected CharSequence decode(Encoding encoding, CharSequence encoded, Charset charSet) throws IOException {
        return encoding.decode(encoded, charSet);
    }

    protected String[] getSingleMatch(final CharSequence input, final boolean relaxed) {
        final String ret[];
        if (relaxed) {
            ret = new Regex(input, "^" + PATTERN_RELAXED + "$").getRow(0);
        } else {
            ret = new Regex(input, "^" + PATTERN_STRICT + "$").getRow(0);
        }
        return ret;
    }

    protected Result decodeSingle(final CharSequence input, final boolean relaxed, final boolean throwException) throws IOException {
        final String[] rfc2047 = getSingleMatch(input, relaxed);
        if (rfc2047 != null) {
            try {
                final Encoding encoding = Encoding.valueOf(rfc2047[3].toUpperCase(Locale.ENGLISH));
                final String in = rfc2047[0];
                String charSetString = rfc2047[2].trim();
                int index = charSetString.indexOf('*');
                if (index != -1) {
                    // remove language specification
                    charSetString = charSetString.substring(0, index);
                }
                final Charset charSet = Charset.forName(charSetString);
                final String encoded = rfc2047[4];
                final CharSequence out = decode(encoding, encoded, charSet);
                return new Result() {

                    @Override
                    public Encoding getEncoding() {
                        return encoding;
                    }

                    @Override
                    public CharSequence getInput() {
                        return in;
                    }

                    @Override
                    public CharSequence getOutput() {
                        return out;
                    }

                    @Override
                    public Charset getCharset() {
                        return charSet;
                    }

                    @Override
                    public CharSequence getEncoded() {
                        return encoded;
                    }

                };
            } catch (final IOException e) {
                if (throwException) {
                    throw e;
                }
            } catch (final Exception e) {
                if (throwException) {
                    throw new IOException(e);
                }
            }
        }
        return null;
    }
}
