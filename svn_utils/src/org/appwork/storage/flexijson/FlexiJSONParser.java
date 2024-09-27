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
package org.appwork.storage.flexijson;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.WeakHashMap;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.utils.IO;
import org.appwork.utils.IO.BOM;
import org.appwork.utils.IO.BOM.BOMInputStream;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 */
public class FlexiJSONParser {
    static final HashSet<ParsingError>        IGNORE_LIST_ALL                   = new HashSet<ParsingError>();
    public static final HashSet<ParsingError> IGNORE_LIST_JS                    = new HashSet<ParsingError>();
    // https://json5.org/
    // MISSING: Strings may span multiple lines by escaping new line characters.
    // MISSING: Strings may include character escapes.
    // public static final HashSet<ParsingError> IGNORE_LIST_RFC_7159 = new HashSet<ParsingError>();
    public static final HashSet<ParsingError> IGNORE_LIST_ENSURE_CORRECT_VALUES = new HashSet<ParsingError>();
    public static final HashSet<ParsingError> IGNORE_LIST_EXTRA_COMMAS          = new HashSet<ParsingError>();
    public static final HashSet<ParsingError> IGNORE_LIST_COMMENTS              = new HashSet<ParsingError>();
    static {
        // ensure init to fill the sets above
        ParsingError.values();
        for (ParsingError e : ParsingError.values()) {
            FlexiJSONParser.IGNORE_LIST_ALL.add(e);
        }
        {
            IGNORE_LIST_COMMENTS.add(ParsingError.ERROR_INLINE_COMMENT);
            IGNORE_LIST_COMMENTS.add(ParsingError.ERROR_LINE_COMMENT);
        }
        {
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_LEADING_COMMA_IN_ARRAY);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_LEADING_COMMA_IN_OBJECT);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_TRAILING_COMMA_IN_ARRAY);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_TRAILING_COMMA_IN_OBJECT);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY);
            IGNORE_LIST_EXTRA_COMMAS.add(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT);
        }
        {
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_IS_BOOLEAN);
            IGNORE_LIST_JS.add(ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_IS_NULL);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_IS_NUMBER);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_IS_BOOLEAN);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_IS_UNDEFINED);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_WITH_SINGLE_QUOTES);
            IGNORE_LIST_JS.add(ParsingError.ERROR_KEY_WITHOUT_QUOTES);
            IGNORE_LIST_JS.add(ParsingError.ERROR_NUMBER_NAN);
            IGNORE_LIST_JS.add(ParsingError.ERROR_STRING_TOKEN_WITHOUT_QUOTES);
            IGNORE_LIST_JS.add(ParsingError.ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
            IGNORE_LIST_JS.add(ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES);
            IGNORE_LIST_JS.add(ParsingError.ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        }
        {
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_KEY_WITHOUT_QUOTES);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_INLINE_COMMENT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_KEY_IS_NUMBER);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_KEY_WITH_SINGLE_QUOTES);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_LEADING_COMMA_IN_ARRAY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_LEADING_COMMA_IN_OBJECT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_LEADING_DECIMAL_POINT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_LINE_COMMENT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBER_INFINITY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBER_NAN);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBERFORMAT_BINARY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBERFORMAT_HEXADECIMAL);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBERFORMAT_LEADING_PLUS);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_NUMBERFORMAT_OCTAL);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_STRING_TOKEN_WITHOUT_QUOTES);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_TRAILING_COMMA_IN_ARRAY);
            IGNORE_LIST_ENSURE_CORRECT_VALUES.add(ParsingError.ERROR_TRAILING_COMMA_IN_OBJECT);
        }
    }
    /**
     *
     */
    private static final char SQUARE_BRACKET_CLOSE = ']';
    /**
     *
     */
    private static final char CURLY_BRACKET_CLOSE  = '}';
    /**
     *
     */
    private static final char COLON                = ':';
    /**
     *
     */
    private static final char ASTERISK             = '*';
    /**
     *
     */
    private static final char SLASH                = '/';
    /**
     *
     */
    private static final char SQUARE_BRACKET_OPEN  = '[';
    /**
     *
     */
    private static final char CURLY_BRACKET_OPEN   = '{';
    /**
     *
     */
    private static final char COMMA                = ',';

    public class NumberParser {
        /**
         *
         */
        private static final char ZERO  = '0';
        /**
         *
         */
        private static final char MINUS = '-';
        /**
         *
         */
        private static final char PLUS  = '+';
        private boolean           bin;
        private int               digitsStartIndex;
        protected boolean         firstIsZero;
        private boolean           hasDigit;
        private boolean           hasPoint;
        private boolean           hasPot;
        private boolean           hex;
        protected int             index = 0;
        private StringParser      Infinity;
        private boolean           invalid;
        private boolean           leadingMinus;
        private boolean           leadingPlus;
        private StringParser      NaN;
        private boolean           oct;
        // private String error;
        private int               trailingWhitespace;

        public NumberParser() {
            NaN = new StringParser("NaN".toCharArray());
            Infinity = new StringParser("Infinity".toCharArray());
        }

        /**
         * checks if we can add the char WITHOUT changing the internal state
         *
         * @param c
         * @return
         */
        public boolean check(char c) {
            if (invalid) {
                return false;
            }
            if (index == 0) {
                if (c == PLUS) {
                    return true;
                } else if (c == MINUS) {
                    return true;
                }
            }
            if (Character.isWhitespace(c)) {
                if (hasDigit) {
                    if (trailingWhitespace < 0) {
                        trailingWhitespace = index;
                    }
                    return true;
                } else {
                    // whitespace beween +|i and the digits
                    return true;
                }
            } else if (trailingWhitespace >= 0) {
                // content after trailing whitespace
                return false;
            }
            if (NaN.check(c)) {
                return true;
            }
            if (Infinity.check(c)) {
                return true;
            }
            int localStartIndex = digitsStartIndex;
            if (localStartIndex < 0) {
                localStartIndex = index;
            }
            if (!hasPoint) {
                if (localStartIndex == index && c == ZERO) {
                } else if (index == localStartIndex + 1 && firstIsZero) {
                    if (c == 'x') {
                        return true;
                    } else if (c == 'b') {
                        return true;
                    }
                }
            }
            if (Character.isDigit(c)) {
                if (oct && (c == '8' || c == '9')) {
                    return false;
                }
                if (bin && c != ZERO && c != '1') {
                    return false;
                }
                return true;
            }
            if (hex) {
                switch (c) {
                case 'a':
                case 'A':
                case 'b':
                case 'B':
                case 'c':
                case 'C':
                case 'd':
                case 'D':
                case 'e':
                case 'E':
                case 'f':
                case 'F':
                    return true;
                default:
                    return false;
                }
            }
            if (c == '.') {
                if (!hasPoint) {
                    if (hex || bin) {
                        return false;
                    }
                    if (oct && index > digitsStartIndex + 1) {
                        return false;
                    }
                    return true;
                } else {
                    return false;
                }
            }
            if (hasPoint && !hasPot) {
                if (c == 'E' || c == 'e') {
                    if (oct || hex || bin) {
                        return false;
                    }
                    return true;
                }
            }
            if (hasPot) {
                if (c == PLUS || c == MINUS) {
                    return true;
                }
            }
            return false;
        }

        protected Number parseFixedNumber(final CharSequence charSequence, final int radix) {
            final String string = charSequence.toString();
            // TODO: Add Java9 Long.valueOf(CharSequence)
            final Long ret = Long.valueOf(string, radix);
            if (ret.longValue() <= Integer.MAX_VALUE && ret.longValue() >= Integer.MIN_VALUE) {
                if (ret.longValue() <= Short.MAX_VALUE && ret.longValue() >= Short.MIN_VALUE) {
                    if (ret.longValue() <= Byte.MAX_VALUE && ret.longValue() >= Byte.MIN_VALUE) {
                        return Byte.valueOf(ret.byteValue());
                    } else {
                        return Short.valueOf(ret.shortValue());
                    }
                } else {
                    return Integer.valueOf(ret.intValue());
                }
            }
            return ret;
        }

        protected Number parseSmallestFixedNumberType(CharSequence value, final int radix, final boolean leadingMinus) {
            if (leadingMinus) {
                return parseFixedNumber("-" + value, radix);
            } else {
                return parseFixedNumber(value, radix);
            }
        }

        protected Number parseSmalltestFloatNumberType(CharSequence value, final boolean leadingMinus) {
            final String string = value.toString();
            double num = Double.parseDouble(string);
            if (leadingMinus) {
                num = -num;
            }
            final float check = (float) num;
            if (check == num && Float.toString(check).length() >= string.length()) {
                return Float.valueOf(check);
            } else {
                return Double.valueOf(num);
            }
        }

        /**
         * @param string
         * @param path
         * @return
         * @throws FlexiParserException
         */
        public Object getValue(String string, Object path) throws FlexiParserException {
            // trailingWhitespace trim trailing whitespace
            if ("0".equals(string)) {
                return 0;
            }
            if (leadingPlus) {
                throwParserExceptionInternal(ParsingError.ERROR_NUMBERFORMAT_LEADING_PLUS, path, null, container, null);
            }
            if (NaN.isFinished()) {
                throwParserExceptionInternal(ParsingError.ERROR_NUMBER_NAN, path, null, container, null);
                return Double.NaN;
            }
            if (Infinity.isFinished()) {
                throwParserExceptionInternal(ParsingError.ERROR_NUMBER_INFINITY, path, null, container, null);
                if (leadingMinus) {
                    return Double.NEGATIVE_INFINITY;
                } else {
                    return Double.POSITIVE_INFINITY;
                }
            }
            if (bin) {
                throwParserExceptionInternal(ParsingError.ERROR_NUMBERFORMAT_BINARY, path, null, container, null);
                // trim leading 0b and plus/minus/whitespace
                return parseSmallestFixedNumberType(string.substring(digitsStartIndex + 2, trailingWhitespace >= 0 ? trailingWhitespace : string.length()), 2, leadingMinus);
            } else if (oct) {
                if ((trailingWhitespace >= 0 ? trailingWhitespace : string.length()) - digitsStartIndex > 1) {
                    // for 1 the value is 0 which is also dec format
                    throwParserExceptionInternal(ParsingError.ERROR_NUMBERFORMAT_OCTAL, path, null, container, null);
                    // trim leading 0 and plus/minus/whitespace
                    return parseSmallestFixedNumberType(string.substring(digitsStartIndex + 1, trailingWhitespace >= 0 ? trailingWhitespace : string.length()), 8, leadingMinus);
                }
            } else if (hex) {
                throwParserExceptionInternal(ParsingError.ERROR_NUMBERFORMAT_HEXADECIMAL, path, null, container, null);
                // trim leading 0x and plus/minus/whitespace
                return parseSmallestFixedNumberType(string.substring(digitsStartIndex + 2, trailingWhitespace >= 0 ? trailingWhitespace : string.length()), 16, leadingMinus);
            }
            if (hasPoint) {
                // trim plus/minus/whitespace
                String trimmed = string.substring(digitsStartIndex, trailingWhitespace >= 0 ? trailingWhitespace : string.length());
                if (trimmed.charAt(0) == '.') {
                    throwParserExceptionInternal(ParsingError.ERROR_LEADING_DECIMAL_POINT, path, null, container, null);
                }
                if (trimmed.charAt(trimmed.length() - 1) == '.') {
                    throwParserExceptionInternal(ParsingError.ERROR_TRAILING_DECIMAL_POINT, path, null, container, null);
                }
                return parseSmalltestFloatNumberType(trimmed, leadingMinus);
            }
            // trim plus/minus/whitespace
            return parseSmallestFixedNumberType(string.substring(digitsStartIndex, trailingWhitespace >= 0 ? trailingWhitespace : string.length()), 10, leadingMinus);
        }

        /**
         * @return
         */
        public boolean isFinished() {
            if (NaN.isFinished()) {
                return true;
            } else if (Infinity.isFinished()) {
                return true;
            } else {
                return !invalid && hasDigit;
            }
        }

        public void reset() {
            leadingPlus = false;
            leadingMinus = false;
            hex = false;
            oct = false;
            bin = false;
            hasPoint = false;
            hasDigit = false;
            trailingWhitespace = -1;
            hasPot = false;
            NaN.reset();
            Infinity.reset();
            firstIsZero = false;
            index = 0;
            invalid = false;
            digitsStartIndex = -1;
            // error = null;
        }

        /**
         * @param ac
         * @return
         */
        public boolean validate(char c) {
            if (invalid) {
                return false;
            }
            try {
                if (index == 0) {
                    if (c == PLUS) {
                        leadingPlus = true;
                        return true;
                    } else if (c == MINUS) {
                        leadingMinus = true;
                        return true;
                    }
                }
                if (Character.isWhitespace(c)) {
                    if (hasDigit) {
                        if (trailingWhitespace < 0) {
                            trailingWhitespace = index;
                        }
                        return true;
                    } else {
                        // whitespace beween +|i and the digits
                        return true;
                    }
                } else if (trailingWhitespace >= 0) {
                    // content after trailing whitespace
                    invalid = true;
                    return false;
                }
                if (NaN.validate(c)) {
                    return true;
                }
                if (Infinity.validate(c)) {
                    return true;
                }
                if (digitsStartIndex < 0) {
                    digitsStartIndex = index;
                }
                if (!hasPoint) {
                    if (digitsStartIndex == index && c == ZERO) {
                        firstIsZero = true;
                        oct = true;
                    } else if (index == digitsStartIndex + 1 && firstIsZero) {
                        if (c == 'x') {
                            hex = true;
                            oct = false;
                            return true;
                        } else if (c == 'b') {
                            bin = true;
                            oct = false;
                            return true;
                        }
                    }
                }
                if (Character.isDigit(c)) {
                    if (oct && (c == '8' || c == '9')) {
                        // error = "'" + c + "' is not oct format";
                        invalid = true;
                        return false;
                    }
                    if (bin && c != ZERO && c != '1') {
                        // error = "'" + c + "' is not bin format";
                        invalid = true;
                        return false;
                    }
                    hasDigit = true;
                    return true;
                }
                if (hex) {
                    switch (c) {
                    case 'a':
                    case 'A':
                    case 'b':
                    case 'B':
                    case 'c':
                    case 'C':
                    case 'd':
                    case 'D':
                    case 'e':
                    case 'E':
                    case 'f':
                    case 'F':
                        hasDigit = true;
                        return true;
                    }
                }
                if (c == '.') {
                    if (!hasPoint) {
                        hasPoint = true;
                        if (hex || bin) {
                            invalid = true;
                            // error = "'" + c + "' is not allowed for hex or binary format";
                            return false;
                        }
                        if (oct && index > digitsStartIndex + 1) {
                            invalid = true;
                            // error = "'" + c + "' is not allowed for octal format";
                            return false;
                        }
                        // no oct but floating point number
                        oct = false;
                        return true;
                    } else {
                        invalid = true;
                        return false;
                    }
                }
                if (hasPoint && !hasPot) {
                    if (c == 'E' || c == 'e') {
                        if (oct || hex || bin) {
                            // error = "'" + c + "' is not allowed for oct/hex/bin format";
                            invalid = true;
                            return false;
                        }
                        hasPot = true;
                        return true;
                    }
                }
                if (hasPot) {
                    if (c == PLUS || c == MINUS) {
                        return true;
                    }
                }
                invalid = true;
                return false;
            } finally {
                index++;
            }
        }
        /**
         *
         */
    }

    public class ReferenceParser {
        private int     index;
        private boolean invalid;
        private int     finished = 0;
        private String  str      = "";

        public ReferenceParser() {
        }

        /**
         * @param c
         * @return
         */
        private boolean check(char c, boolean setFinished) {
            if (invalid) {
                return false;
            }
            str += c;
            // no isFinished
            if (finished > 0 && !Character.isWhitespace(c)) {
                return false;
            } else if (isFinished() && Character.isWhitespace(c)) {
                return true;
            }
            if (index == 0) {
                if (c == '$') {
                    return true;
                }
            } else if (index == 1) {
                if (c == '{') {
                    return true;
                }
            } else {
                if (c == '}') {
                    if (index <= 2) {
                        // ${}
                        return false;
                    }
                    if (setFinished) {
                        finished = index;
                    }
                    return true;
                } else {
                    if (Character.isAlphabetic(c)) {
                        return true;
                    }
                    if (Character.isDigit(c)) {
                        return true;
                    }
                    switch (c) {
                    case '[':
                    case ']':
                    case '.':
                        return true;
                    }
                }
            }
            return false;
        }

        /**
         * @return
         */
        public boolean isFinished() {
            return !invalid && finished > 0;
        }

        public void reset() {
            index = 0;
            invalid = false;
            finished = 0;
        }

        /**
         * @param ac
         * @return
         */
        public boolean validate(char c) {
            if (invalid) {
                return false;
            }
            if (check(c, true)) {
                index++;
                return true;
            }
            invalid = true;
            return false;
        }

        /**
         *
         */
        /**
         * @param string
         * @param path
         * @return
         */
        public String getValue(String string, Object path) {
            return string.substring(2, finished);
        }

        /**
         * @return
         */
        public boolean mayBeALink() {
            return check('}', false);
        }
    }

    public class StringParser {
        private final char[] expected;
        private int          index;
        private boolean      invalid;

        public StringParser(char[] e) {
            this.expected = e;
        }

        /**
         * @param c
         * @return
         */
        public boolean check(char c) {
            if (invalid) {
                return false;
            }
            if (index < expected.length && expected[index] == c) {
                return true;
            }
            if (isFinished() && Character.isWhitespace(c)) {
                return true;
            }
            return false;
        }

        /**
         * @return
         */
        public boolean isFinished() {
            return !invalid && index >= expected.length;
        }

        public void reset() {
            index = 0;
            invalid = false;
        }

        /**
         * @param ac
         * @return
         */
        public boolean validate(char c) {
            if (check(c)) {
                index++;
                return true;
            }
            invalid = true;
            return false;
        }

        /**
         *
         */
        /**
         * @param string
         * @param path
         * @return
         */
        public Object getValue(String string, Object path) {
            return null;
        }
    }

    /**
     * Warning... tokens are sorted based on their relevance. Do not change
     *
     * @author thomas
     * @date 18.03.2021
     *
     */
    public static enum Token {
        ASSIGN,
        COMMA,
        COMMENT_INLINE,
        COMMENT_LINE,
        END_OF_ARRAY,
        END_OF_OBJECT,
        KEY,
        START_OF_ARRAY,
        START_OF_OBJECT,
        VALUE,
        VALUE_BOOLEAN,
        VALUE_NULL,
        VALUE_NUMBER,
        VALUE_STRING,
        WS_AFTER_ASSIGN,
        WS_AFTER_KEY,
        WS_AFTER_VALUE,
        WS_BEFORE_KEY,
        WS_BEFORE_VALUE;
    }

    private static final char                                       BACKSLASH            = '\\';
    /**
     *
     */
    private static final char                                       BACKSPACE            = '\b';
    /**
     *
     */
    private static final char                                       CARRIAGE_RETURN      = '\r';
    /**
     *
     */
    private static final char[]                                     CHAR_ARRAY_FALSE     = "false".toCharArray();
    /**
     *
     */
    private static final char[]                                     CHAR_ARRAY_NAN       = "NaN".toCharArray();
    private static final char[]                                     CHAR_ARRAY_UNDEFINED = "undefined".toCharArray();
    /**
     *
     */
    private static final char[]                                     CHAR_ARRAY_NULL      = "null".toCharArray();
    /**
     *
     */
    private static final char[]                                     CHAR_ARRAY_TRUE      = "true".toCharArray();
    private final static WeakHashMap<String, WeakReference<String>> DEDUPEMAP            = new WeakHashMap<String, WeakReference<String>>();
    /**
     *
     */
    private static final char                                       DOUBLE_QUOTE         = '"';
    protected static final char                                     END_OF_TRANSMISSION  = '\u2404';
    // public static final FlexiJSonNode FALSE = new FlexiJSonValue(false);
    /**
     *
     */
    private static final char                                       FORM_FEED            = '\f';
    /**
     *
     */
    private static final char                                       LINE_FEED            = '\n';
    // public static final FlexiJSonNode NULL = new FlexiJSonValue(null);
    /**
     *
     */
    private static final char                                       SINGLE_QUOTE         = '\'';
    /**
     *
     */
    private static final char                                       TAB                  = '\t';

    public int getIndex() {
        return index;
    }

    // public static final FlexiJSonNode TRUE = new FlexiJSonValue(true);
    public static String dedupeString(String string) {
        if (string != null) {
            synchronized (DEDUPEMAP) {
                String ret = null;
                WeakReference<String> ref = DEDUPEMAP.get(string);
                if (ref != null && (ret = ref.get()) != null) {
                    return ret;
                }
                ref = new WeakReference<String>(string);
                DEDUPEMAP.put(string, ref);
                return string;
            }
        } else {
            return null;
        }
    }

    private char[]               aheadBuffer;
    private final char[]         buffer       = new char[1];
    private FlexiJSonNode        container;
    StringBuilder                debug        = null;
    public HashSet<ParsingError> ignoreIssues = new HashSet<ParsingError>();
    protected int                index        = 0;
    private StringParser         isFalse;
    private StringParser         isNull;
    private NumberParser         isNumber;
    private StringParser         isTrue;
    private ReferenceParser      isReference;
    private int                  lastNonWhitespaceIndex;
    private Reader               reader       = null;
    final StringBuilder          sb;
    private final StringBuilder  sb2;
    private Token                token;
    private StringParser         isUndefined;

    public static enum StringQuoting {
        NONE,
        DOUBLE,
        SINGLE
    }

    protected StringQuoting stringQuoting = StringQuoting.NONE;

    /**
     * @param byteArrayInputStream
     */
    public FlexiJSONParser(InputStream is) {
        this.reader = wrap(this, is);
        sb = new StringBuilder();
        sb2 = new StringBuilder();
        initParsers();
    }

    private static Reader wrap(final FlexiJSONParser parser, final InputStream is) {
        return new Reader() {
            volatile Reader reader = null;

            private Reader getReader() throws IOException {
                if (reader == null) {
                    final BOMInputStream bis = IO.BOM.wrap(is);
                    final BOM bom = bis.getBOM();
                    if (bom != null) {
                        reader = new InputStreamReader(bis, bom.getCharSet());
                    } else {
                        reader = new InputStreamReader(bis, BOM.UTF8.getCharSet());
                    }
                    parser.reader = reader;
                }
                return reader;
            }

            @Override
            public int read(char[] cbuf, int off, int len) throws IOException {
                return getReader().read(cbuf, off, len);
            }

            @Override
            public void close() throws IOException {
                getReader().close();
            }
        };
    }

    /**
     * @param bufferedReader
     */
    public FlexiJSONParser(Reader reader) {
        this.reader = reader;
        sb = new StringBuilder();
        sb2 = new StringBuilder();
        initParsers();
    }

    protected void initParsers() {
        isNumber = new NumberParser();
        isNull = new StringParser(CHAR_ARRAY_NULL);
        isUndefined = new StringParser(CHAR_ARRAY_UNDEFINED);
        isTrue = new StringParser(CHAR_ARRAY_TRUE);
        isFalse = new StringParser(CHAR_ARRAY_FALSE);
        isReference = new ReferenceParser();
    }

    private boolean parseReferencesEnabled = false;

    public boolean isParseReferencesEnabled() {
        return parseReferencesEnabled;
    }

    public void setParseReferencesEnabled(boolean parseReferencesEnabled) {
        this.parseReferencesEnabled = parseReferencesEnabled;
    }

    public FlexiJSONParser(final String json) {
        this(new StringReader(json));
    }

    public FlexiJSONParser(final CharSequence json) {
        this(new CharSequenceReader(json));
    }

    protected void appendC(char c) {
        isNumber.validate(c);
        isNull.validate(c);
        isTrue.validate(c);
        isFalse.validate(c);
        isUndefined.validate(c);
        if (isParseReferencesEnabled()) {
            isReference.validate(c);
        }
        if (tokenParserExtensions != null) {
            for (FlexiJSONParserExtension sp : tokenParserExtensions) {
                sp.validate(c);
            }
        }
        sb.append(c);
        if (!Character.isWhitespace(c)) {
            lastNonWhitespaceIndex = sb.length();
        }
    }

    protected Object assignFinalType(Object path, Token type) throws FlexiParserException {
        // if (isNaN.isFinished()) {
        // throwParserExceptionInternal(ParsingError.ERROR_NUMBER_NAN, path, null, container, null);
        // return Double.NaN;
        // }
        String str = null;
        if (tokenParserExtensions != null) {
            for (FlexiJSONParserExtension sp : tokenParserExtensions) {
                if (str == null) {
                    str = dedupeString(sb.toString());
                }
                if (type == Token.KEY) {
                    FinalKey ret = sp.getKey(this, str, path, stringQuoting, currentChar(path));
                    if (ret != null) {
                        if (StringUtils.equals(ret.key, str)) {
                        }
                        return ret;
                    }
                } else {
                    FlexiJSonNode ret = sp.getValue(this, str, path, stringQuoting, currentChar(path));
                    if (ret != null) {
                        return ret;
                    }
                }
            }
        }
        if (isParseReferencesEnabled() && isReference.isFinished()) {
            return this.createJSonReference(isReference.getValue(sb.toString(), path));
        }
        switch (stringQuoting) {
        case DOUBLE:
            if (currentChar(path) != DOUBLE_QUOTE) {
                throwParserExceptionInternal(ParsingError.EXPECTED_STRING_OR_DOUBLE_QUOTE, path, null, container, null);
            }
            if (str == null) {
                str = dedupeString(sb.toString());
            }
            return str;
        case SINGLE:
            if (currentChar(path) != SINGLE_QUOTE) {
                throwParserExceptionInternal(ParsingError.EXPECTED_STRING_OR_SINGLE_QUOTE, path, null, container, null);
            }
            if (str == null) {
                str = dedupeString(sb.toString());
            }
            return str;
        default:
            // nothing
        }
        if (isFalse.isFinished()) {
            return Boolean.FALSE;
        }
        if (isTrue.isFinished()) {
            return Boolean.TRUE;
        }
        if (isUndefined.isFinished()) {
            return null;
        }
        if (isNull.isFinished()) {
            return null;
        }
        if (isNumber.isFinished()) {
            try {
                if (str == null) {
                    str = sb.toString();
                }
                return isNumber.getValue(str, path);
            } catch (NumberFormatException e) {
                // should not happen - even in case of bad formated json. isNumber should validate the number
                throw new WTFException(e);
            } catch (StringIndexOutOfBoundsException e) {
                throw e;
            }
        }
        if (sb.length() == 0) {
            return createJSonValue();
        }
        throwParserExceptionInternal(ParsingError.ERROR_STRING_TOKEN_WITHOUT_QUOTES, path, null, container, null);
        return dedupeString(sb.substring(0, lastNonWhitespaceIndex));
    }

    /**
     * @param value
     * @return
     */
    private FlexiJSonValue createJSonReference(String value) {
        return new FlexiJSonValue(value, true);
    }

    public FlexiJSonArray createJSonArray() {
        return new FlexiJSonArray();
    }

    public FlexiJSonObject createJSonObject() {
        return new FlexiJSonObject();
    }

    /**
     * @return
     */
    public FlexiJSonValue createJSonValue() {
        return new FlexiJSonValue();
    }

    public FlexiJSonValue createJSonValue(Number longValue) {
        return new FlexiJSonValue(longValue);
    }

    public FlexiJSonValue createJSonValue(Boolean value) {
        return new FlexiJSonValue(value);
    }

    /**
     * @param object
     * @return
     */
    public FlexiJSonValue createJSonValue(String value) {
        if (value == null) {
            return new FlexiJSonValue((String) null);
        } else {
            return new FlexiJSonValue(dedupeString(value));
        }
    }

    /**
     * @param path
     * @return
     */
    protected char currentChar(Object path) {
        return buffer[0];
    }

    /**
     * override to track the path. disabled by default due to performance reasons
     *
     * @param path
     * @param key
     * @return
     */
    protected Object extendPath(Object path, Object key) {
        return null;
    }

    public StringBuilder getDebug() {
        return debug;
    }

    public HashSet<ParsingError> getIgnoreIssues() {
        return ignoreIssues;
    }

    public Token getToken() {
        return token;
    }

    protected boolean isEndOfStreamReached() {
        return currentChar(null) == END_OF_TRANSMISSION;
    }

    /**
     * @param path
     * @return
     * @throws FlexiParserException
     */
    private char lookahead(Object path) throws FlexiParserException {
        if (aheadBuffer != null) {
            return aheadBuffer[0];
        } else {
            aheadBuffer = new char[1];
            try {
                if (reader.read(aheadBuffer) != 1) {
                    aheadBuffer[0] = END_OF_TRANSMISSION;
                }
            } catch (IOException e) {
                throwParserExceptionInternal(ParsingError.ERROR_IO_EXCEPTION_IN_STREAM, path, e, container, null);
            }
            return aheadBuffer[0];
        }
    }

    /**
     * @param key
     * @return
     */
    protected String mapKey(String key) {
        return key;
    }

    protected void nextAtEndOfToken(Object path, FlexiJSonNode parent) throws FlexiParserException {
        if (parent instanceof FlexiJSonObject) {
            nextChar(path, ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG);
        } else if (parent instanceof FlexiJSonArray) {
            nextChar(path, ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG);
        } else {
            nextChar(path, null);
        }
    }

    /**
     * @param expected
     * @return
     * @throws FlexiParserException
     *
     */
    protected boolean nextChar(Object path, ParsingError expected) throws FlexiParserException {
        try {
            if (buffer[0] == END_OF_TRANSMISSION) {
                return false;
            }
            if (readNextChar() == 1) {
                index++;
                if (debug != null) {
                    debug.append(buffer[0]);
                }
                return true;
            }
        } catch (IOException e) {
            throwParserExceptionInternal(ParsingError.ERROR_IO_EXCEPTION_IN_STREAM, path, e, container, null);
        }
        if (expected != null) {
            throwParserExceptionInternal(expected, path, null, container, null);
        }
        buffer[0] = END_OF_TRANSMISSION;
        return false;
    }

    /**
     * override to evaluate comments
     *
     * @param path
     * @param ret
     * @param string
     */
    protected void onComment(Object path, FlexiJSonNode ret, String string) {
        // System.out.println("Comment " + string);
    }

    public FlexiJSonNode parse() throws FlexiParserException {
        // array: support for multiple values in a row [1][2]
        FlexiJSonArray array = createJSonArray();
        try {
            nextChar(null, ParsingError.EXPECTED_ANY_CHARACTER);
            while (true) {
                skipWhitespace(null, ParsingError.EXPECTED_ANY_CHARACTER, Token.WS_BEFORE_VALUE);
                final FlexiJSonNode ret = parseValue(null);
                if (ret instanceof FlexiJSonComments) {
                    // Comments at the end of string
                    if (array.size() == 0) {
                        throwParserExceptionInternal(ParsingError.ERROR_NO_CONTENT, null, null, array, null);
                        addToArray(array, createJSonValue());
                    }
                    FlexiJSonNode before = array.get(array.size() - 1);
                    FlexiJSonComments a = before.getCommentsAfter();
                    if (a == null) {
                        a = (FlexiJSonComments) ret;
                    } else {
                        a.addAll((FlexiJSonComments) ret);
                    }
                    before.setCommentsAfter(a);
                    break;
                } else {
                    if (ret != null) {
                        if (array.size() > 0) {
                            throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, null, null, array, null);
                        }
                        addToArray(array, ret);
                        if (isBreakAtEndOfObject()) {
                            break;
                        }
                    }
                    if (!skipWhitespace(null, null, Token.WS_AFTER_VALUE)) {
                        // end of stream
                        break;
                    }
                    throwParserExceptionInternal(ParsingError.ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, null, null, array, null);
                    if (currentChar(null) == COMMA) {
                        nextChar(null, null);
                        if (!skipWhitespace(null, null, Token.WS_BEFORE_VALUE)) {
                            // allow trailing , without adding another element
                            break;
                        }
                    }
                }
            }
        } catch (BreakParsingWithoutException e) {
            // nothing
        }
        if (array.size() == 1) {
            // reset parent
            array.get(0).setParent(null);
            return array.get(0);
        } else {
            return array;
        }
    }

    protected void addToArray(FlexiJSonArray array, final FlexiJSonNode newElement) {
        array.add(newElement);
    }

    protected FlexiJSonArray parseArray(Object path) throws FlexiParserException {
        FlexiJSonNode parent = container;
        try {
            setToken(path, Token.START_OF_ARRAY);
            final FlexiJSonArray ret = createJSonArray();
            container = ret;
            if (!nextChar(path, ParsingError.EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG)) {
                return ret;
            }
            int lastActualValueIndex = -1;
            while (true) {
                // skip whitespace
                if (!skipWhitespace(path, ParsingError.EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG, Token.WS_BEFORE_VALUE)) {
                    return ret;
                }
                FlexiJSonComments beforeValue = readComment(extendPath(path, ret.size()), ret, Token.WS_BEFORE_VALUE);
                FlexiJSonNode value;
                switch (currentChar(path)) {
                case COMMA:
                    // empty array element ,,,,,,]
                    value = createJSonValue();
                    value.addCommentsBefore(beforeValue);
                    addToArray(ret, value);
                    if (ret.size() == 1) {
                        throwParserExceptionInternal(ParsingError.ERROR_LEADING_COMMA_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                    } else if (ret.size() > 1) {
                        if (lastActualValueIndex == -1) {
                            throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                        }
                        throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                    }
                    setToken(extendPath(path, ret.size()), Token.COMMA);
                    if (!nextChar(extendPath(path, ret.size()), ParsingError.EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG)) {
                        return ret;
                    }
                    break;
                case ']':
                    setToken(path, Token.END_OF_ARRAY);
                    ret.addCommentsInside(beforeValue);
                    if (ret.size() - lastActualValueIndex > 1) {
                        throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                        throwParserExceptionInternal(ParsingError.ERROR_TRAILING_COMMA_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                    } else if (ret.size() > 0) {
                        throwParserExceptionInternal(ParsingError.ERROR_TRAILING_COMMA_IN_ARRAY, extendPath(path, ret.size() - 1), null, ret, null);
                    }
                    nextAtEndOfToken(extendPath(path, ret.size() - 1), parent);
                    return ret;
                default:
                    Object newPath = parseArrayValue(ret, path);
                    if (beforeValue != null && beforeValue.size() > 0) {
                        FlexiJSonNode newValue = ret.get(ret.size() - 1);
                        FlexiJSonComments tmp = newValue.getCommentsBefore();
                        if (tmp == null) {
                            newValue.setCommentsBefore(beforeValue);
                        } else {
                            // add before
                            tmp.addAll(0, beforeValue);
                            newValue.setCommentsBefore(tmp);
                        }
                    }
                    lastActualValueIndex = ret.size() - 1;
                    if (!skipWhitespace(newPath, ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG, Token.WS_AFTER_VALUE)) {
                        return ret;
                    }
                    switch (currentChar(newPath)) {
                    case COMMA:
                        setToken(newPath, Token.COMMA);
                        if (!nextChar(newPath, ParsingError.EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG)) {
                            return ret;
                        }
                        break;
                    case ']':
                        setToken(newPath, Token.END_OF_ARRAY);
                        nextAtEndOfToken(newPath, parent);
                        return ret;
                    default:
                        throwParserExceptionInternal(ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG, newPath, null, ret, null);
                        break;
                    }
                }
            }
        } finally {
            container = parent;
        }
    }

    protected Object parseArrayValue(final FlexiJSonArray ret, Object path) throws FlexiParserException {
        Object newPath = path;
        addToArray(ret, parseValue(newPath = extendPath(path, ret.size())));
        return newPath;
    }

    // |WS-KEY-WS-:-WS-value-WS]
    protected boolean parseKeyValuePair(Object newPath, final FlexiJSonObject ret) throws FlexiParserException {
        String key;
        skipWhitespace(newPath, ParsingError.EXPECTED_KEY_OR_OBJECT_END_TAG, Token.WS_BEFORE_KEY);
        FlexiJSonComments commentsBeforeKey = readComment(newPath, container, Token.WS_BEFORE_KEY);
        if (currentChar(newPath) == CURLY_BRACKET_CLOSE) {
            ret.setCommentsInside(commentsBeforeKey);
            return false;
        }
        setToken(newPath, Token.KEY);
        Object keyToken = readToken(newPath, Token.KEY);
        key = modifyKey(String.valueOf(keyToken));
        if (keyToken instanceof FinalKey) {
            // key from extension - no further error handling
            key = modifyKey(String.valueOf(((FinalKey) keyToken).key));
        } else if (keyToken instanceof FlexiJSonNode) {
            // UNDEFINED
            key = "undefined";
            throwParserExceptionInternal(ParsingError.ERROR_KEY_WITHOUT_QUOTES, newPath, null, container, null);
            throwParserExceptionInternal(ParsingError.ERROR_KEY_IS_UNDEFINED, newPath, null, container, null);
        } else if (keyToken == null) {
            throwParserExceptionInternal(ParsingError.ERROR_KEY_WITHOUT_QUOTES, newPath, null, container, null);
            throwParserExceptionInternal(ParsingError.ERROR_KEY_IS_NULL, newPath, null, container, null);
        } else if (keyToken instanceof Boolean) {
            throwParserExceptionInternal(ParsingError.ERROR_KEY_WITHOUT_QUOTES, newPath, null, container, null);
            throwParserExceptionInternal(ParsingError.ERROR_KEY_IS_BOOLEAN, newPath, null, container, null);
        } else if (keyToken instanceof Number) {
            throwParserExceptionInternal(ParsingError.ERROR_KEY_WITHOUT_QUOTES, newPath, null, container, null);
            throwParserExceptionInternal(ParsingError.ERROR_KEY_IS_NUMBER, newPath, null, container, null);
        } else {
            switch (stringQuoting) {
            case SINGLE:
                throwParserExceptionInternal(ParsingError.ERROR_KEY_WITH_SINGLE_QUOTES, newPath, null, container, null);
                break;
            case NONE:
                throwParserExceptionInternal(ParsingError.ERROR_KEY_WITHOUT_QUOTES, newPath, null, container, null);
                break;
            case DOUBLE:
                // nothing
            }
        }
        newPath = extendPath(newPath, key);
        KeyValueElement element = new KeyValueElement(ret, newPath, mapKey(key), null);
        element.addCommentsBeforeKey(commentsBeforeKey, true);
        skipWhitespace(newPath, ParsingError.EXPECTED_ANY_CHARACTER, Token.WS_AFTER_KEY);
        FlexiJSonComments commentsAfterKey = readComment(newPath, container, Token.WS_AFTER_KEY);
        element.addCommentsAfterKey(commentsAfterKey);
        if (skipWhitespaceToChar(newPath, ParsingError.EXPECTED_COLON, Token.WS_AFTER_KEY, COLON)) {
            setToken(newPath, Token.ASSIGN);
            nextChar(newPath, ParsingError.EXPECTED_ANY_VALUE);
        }
        FlexiJSonComments commentsBeforeValue1 = readComment(newPath, container, Token.WS_BEFORE_VALUE);
        if (!skipWhitespace(newPath, ParsingError.EXPECTED_ANY_VALUE, Token.WS_BEFORE_VALUE)) {
            FlexiJSonNode empty = createJSonValue();
            empty.addCommentsBefore(commentsBeforeValue1);
            ret.add(new KeyValueElement(ret, newPath, mapKey(key), empty));
            return false;
        }
        FlexiJSonComments commentsBeforeValue2 = readComment(newPath, container, Token.WS_BEFORE_VALUE);
        setToken(newPath, Token.VALUE);
        FlexiJSonNode newValue = parseValue(newPath);
        FlexiJSonComments aks = element.getCommentsAfterKey();
        FlexiJSonComments bks = element.getCommentsBeforeKey();
        // now that we have the value, set it as parent to the before and after key comments
        if (aks != null) {
            aks.setParent(newValue);
        }
        if (bks != null) {
            bks.setParent(newValue);
        }
        newValue.addCommentsBefore(commentsBeforeValue1);
        newValue.addCommentsBefore(commentsBeforeValue2);
        newValue.addCommentsAfter(readComment(newPath, container, Token.WS_AFTER_VALUE));
        if (newValue instanceof FlexiJSonValue && ((FlexiJSonValue) newValue).getType() == ValueType.UNDEFINED) {
            throwParserExceptionInternal(ParsingError.ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED, newPath, null, container, newValue);
        }
        if (ret.containsKey(element.getKey())) {
            throwParserExceptionInternal(ParsingError.ERROR_DUPLICATED_KEY_PROPERTY, newPath, null, ret, newValue);
        }
        element.setValue(newValue);
        onNewKeyValueElement(ret, element, keyToken);
        // do not remove existing. if we ignore ERROR_DUPLICATED_KEY_PROPERTY we should have it twice in the map
        ret.add(element, false);
        skipWhitespace(newPath, ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, Token.WS_AFTER_VALUE);
        newValue.addCommentsAfter(readComment(newPath, container, Token.WS_AFTER_VALUE));
        return true;
    }

    /**
     * @param ret
     * @param element
     * @param keyToken
     */
    protected void onNewKeyValueElement(FlexiJSonObject ret, KeyValueElement element, Object keyToken) {
    }

    /**
     * @param valueOf
     * @return
     */
    protected String modifyKey(String keyString) {
        return keyString;
    }

    protected FlexiJSonObject parseObject(Object path) throws FlexiParserException {
        FlexiJSonNode parent = container;
        try {
            setToken(path, Token.START_OF_OBJECT);
            final FlexiJSonObject ret = createJSonObject();
            container = ret;
            if (!nextChar(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG)) {
                return ret;
            }
            skipWhitespace(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG, Token.WS_BEFORE_KEY);
            int entryIndex = 0;
            int lastObjectIndex = -1;
            while (true) {
                // check for object end markers
                // do not skip whitespace. this is part of parseKeyValuePair
                FlexiJSonComments beforeElement = readComment(path, ret, Token.WS_BEFORE_KEY);
                switch (currentChar(path)) {
                case END_OF_TRANSMISSION:
                    // this happens for{"value" : 2
                    throwParserExceptionInternal(ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, path, null, ret, null);
                    return ret;
                case COMMA:
                    setToken(path, Token.COMMA);
                    entryIndex++;
                    KeyValueElement empty = new KeyValueElement(ret, path, null, createJSonValue());
                    empty.setCommentsBeforeKey(beforeElement);
                    beforeElement = null;
                    ret.add(empty);
                    // ok another value...probably
                    // {,,,, empty object entries
                    if (entryIndex == 1) {
                        throwParserExceptionInternal(ParsingError.ERROR_LEADING_COMMA_IN_OBJECT, path, null, ret, null);
                    } else if (entryIndex > 1) {
                        if (ret.size() > 0) {
                            throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, path, null, ret, null);
                        } else {
                            throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, path, null, ret, null);
                            throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, path, null, ret, null);
                        }
                    }
                    if (!nextChar(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG)) {
                        return ret;
                    }
                    skipWhitespace(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG, Token.WS_BEFORE_KEY);
                    break;
                case CURLY_BRACKET_CLOSE:
                    setToken(path, Token.END_OF_OBJECT);
                    if (entryIndex == ret.getElements().size() && entryIndex > 0) {
                        empty = new KeyValueElement(ret, path, null, createJSonValue());
                        if (beforeElement != null) {
                            empty.setCommentsBeforeKey(beforeElement);
                        }
                        ret.add(empty);
                    } else if (beforeElement != null) {
                        ret.addCommentsInside(beforeElement);
                    }
                    if (entryIndex - lastObjectIndex > 1) {
                        throwParserExceptionInternal(ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT, path, null, ret, null);
                        throwParserExceptionInternal(ParsingError.ERROR_TRAILING_COMMA_IN_OBJECT, path, null, ret, null);
                    } else if (entryIndex > 0) {
                        throwParserExceptionInternal(ParsingError.ERROR_TRAILING_COMMA_IN_OBJECT, path, null, ret, null);
                    }
                    nextAtEndOfToken(path, parent);
                    return ret;
                default:
                    if (parseKeyValuePair(path, ret)) {
                        lastObjectIndex = entryIndex;
                    }
                    if (beforeElement != null && beforeElement.size() > 0) {
                        ret.last().addCommentsBeforeKey(beforeElement, false);
                    }
                    // no comment reader here. parseKEyValue does the comment parsing
                    switch (currentChar(path)) {
                    case COMMA:
                        entryIndex++;
                        setToken(path, Token.COMMA);
                        if (!nextChar(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG)) {
                            return ret;
                        }
                        skipWhitespace(path, ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG, Token.WS_BEFORE_KEY);
                        break;
                    case CURLY_BRACKET_CLOSE:
                        setToken(path, Token.END_OF_OBJECT);
                        nextAtEndOfToken(path, parent);
                        return ret;
                    default:
                        throwParserExceptionInternal(ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, path, null, ret, null);
                        break;
                    }
                }
            }
        } finally {
            container = parent;
        }
    }

    /**
     * No Whitespace handling in this method. must be called with index on first content char and must end with index with end of content+1
     *
     * @param path
     * @return
     * @throws FlexiParserException
     */
    protected FlexiJSonNode parseValue(Object path) throws FlexiParserException {
        // no skip ws. the method expects to be called with the index on the first value char
        // setToken(path, Token.WS_BEFORE_VALUE);
        // skipWhiteSpace(path, ParsingError.EXPECTED_ANY_VALUE);
        FlexiJSonComments commentsBeforeValue = readComment(path, container, Token.WS_BEFORE_VALUE);
        setToken(path, Token.VALUE);
        FlexiJSonNode value;
        switch (currentChar(path)) {
        case CURLY_BRACKET_OPEN:
            value = parseObject(path);
            break;
        case SQUARE_BRACKET_OPEN:
            value = parseArray(path);
            break;
        case END_OF_TRANSMISSION:
            // happens if: comment at end of stream
            return commentsBeforeValue;
        default:
            Object token = readToken(path, Token.VALUE);
            if (token instanceof FlexiJSonNode) {
                value = (FlexiJSonNode) token;
            } else if (token == null) {
                if (isUndefined.isFinished()) {
                    value = createJSonValue();
                } else {
                    value = createJSonValue((String) null);
                }
            } else if (token instanceof Boolean) {
                value = createJSonValue((Boolean) token);
            } else if (token instanceof Number) {
                value = createJSonValue((Number) token);
            } else {
                value = createJSonValue((String) token);
                if (stringQuoting == StringQuoting.NONE) {
                    throwParserExceptionInternal(ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES, path, null, container, value);
                } else if (stringQuoting == StringQuoting.SINGLE) {
                    throwParserExceptionInternal(ParsingError.ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, path, null, container, value);
                }
            }
        }
        value.addCommentsBefore(commentsBeforeValue);
        skipWhitespace(path, null, Token.WS_AFTER_VALUE);
        value.addCommentsAfter(readComment(path, container, Token.WS_AFTER_VALUE));
        return value;
    }

    /**
     * @param path
     * @param ret
     * @param wsToken
     * @throws FlexiParserException
     */
    protected FlexiJSonComments readComment(Object path, FlexiJSonNode ret, Token wsToken) throws FlexiParserException {
        if (!isParseInlineCommentEnabled() && !isParseLineCommentEnabled()) {
            return null;
        }
        FlexiJSonComments comments = null;
        if (currentChar(path) == SLASH) {
            int start = index - 1;
            if (lookahead(path) == SLASH && isParseLineCommentEnabled()) {
                // comment until end of line
                throwParserExceptionInternal(ParsingError.ERROR_LINE_COMMENT, path, null, container, null);
                setToken(path, Token.COMMENT_LINE);
                // since we did a lookahead, this cannot result in end of stream
                nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                sb.setLength(0);
                while (true) {
                    nextAtEndOfToken(path, container);
                    switch (currentChar(path)) {
                    case END_OF_TRANSMISSION:
                        onComment(path, ret, sb.toString());
                        if (comments == null) {
                            comments = createCommentsContainer();
                        }
                        comments.add(createComment(cleanComment(sb.toString()), FlexiComment.Type.LINE, FlexiMapperTags.UNKNOWN, start, index, path));
                        return comments;
                    case LINE_FEED:
                    case CARRIAGE_RETURN:
                        onComment(path, ret, sb.toString());
                        if (comments == null) {
                            comments = createCommentsContainer();
                        }
                        comments.add(createComment(cleanComment(sb.toString()), FlexiComment.Type.LINE, FlexiMapperTags.UNKNOWN, start, index - 1, path));
                        if (container instanceof FlexiJSonObject) {
                            skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, wsToken);
                        } else if (container instanceof FlexiJSonArray) {
                            skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG, wsToken);
                        } else {
                            skipWhitespace(path, null, wsToken);
                        }
                        // maybe another comment
                        comments.addAll(readComment(path, ret, wsToken));
                        return comments;
                    default:
                        sb.append(currentChar(path));
                    }
                }
            } else if (lookahead(path) == ASTERISK && isParseInlineCommentEnabled()) {
                // comment until */
                //
                start = index - 1;
                // since we did a lookahead, this cannot result in end of stream
                throwParserExceptionInternal(ParsingError.ERROR_INLINE_COMMENT, path, null, container, null);
                setToken(path, Token.COMMENT_INLINE);
                nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                sb.setLength(0);
                while (true) {
                    nextAtEndOfToken(path, container);
                    if (currentChar(path) == END_OF_TRANSMISSION) {
                        throwParserExceptionInternal(ParsingError.EXPECTED_ANY_CHARACTER_OR_COMMENT_CLOSE_TAGS, path, null, container, null);
                        onComment(path, ret, sb.toString());
                        if (comments == null) {
                            comments = createCommentsContainer();
                        }
                        comments.add(createComment(cleanComment(sb.toString()), FlexiComment.Type.INLINE, FlexiMapperTags.UNKNOWN, start, index, path));
                        return comments;
                    }
                    if (currentChar(path) == ASTERISK && lookahead(path) == SLASH) {
                        // since we did a lookahead, this cannot result in end of stream
                        nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                        int end = index;
                        nextAtEndOfToken(path, container);
                        onComment(path, ret, sb.toString());
                        if (comments == null) {
                            comments = createCommentsContainer();
                        }
                        comments.add(createComment(cleanComment(sb.toString()), FlexiComment.Type.INLINE, FlexiMapperTags.UNKNOWN, start, end, path));
                        if (container instanceof FlexiJSonObject) {
                            skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, wsToken);
                        } else if (container instanceof FlexiJSonArray) {
                            skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG, wsToken);
                        } else {
                            skipWhitespace(path, null, wsToken);
                        }
                        // maybe another comment
                        comments.addAll(readComment(path, ret, wsToken));
                        return comments;
                    }
                    // skip" * " inline multiline comment
                    if (currentChar(path) == CARRIAGE_RETURN) {
                        StringBuilder tmp = new StringBuilder();
                        tmp.append(currentChar(path));
                        nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                        if (currentChar(path) == LINE_FEED) {
                            tmp.append(currentChar(path));
                            nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                        }
                        do {
                            if (isEndOfStreamReached()) {
                                throwParserExceptionInternal(ParsingError.EXPECTED_ANY_CHARACTER, path, null, container, null);
                            }
                            if (Character.isWhitespace(currentChar(path))) {
                                tmp.append(currentChar(path));
                            } else {
                                break;
                            }
                            nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                        } while (true);
                        if (currentChar(path) == ASTERISK) {
                            tmp.append(currentChar(path));
                            // nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                            if (Character.isWhitespace(lookahead(path))) {
                                nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                                // System.out.println(1);
                            } else if (lookahead(path) == SLASH) {
                                // end of comment.
                                // since we did a lookahead, this cannot result in end of stream
                                nextChar(path, ParsingError.EXPECTED_ANY_CHARACTER);
                                int end = index;
                                nextAtEndOfToken(path, container);
                                onComment(path, ret, sb.toString());
                                if (comments == null) {
                                    comments = createCommentsContainer();
                                }
                                comments.add(createComment(cleanComment(sb.toString()), FlexiComment.Type.INLINE, FlexiMapperTags.UNKNOWN, start, end, path));
                                if (container instanceof FlexiJSonObject) {
                                    skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, wsToken);
                                } else if (container instanceof FlexiJSonArray) {
                                    skipWhitespace(path, ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG, wsToken);
                                } else {
                                    skipWhitespace(path, null, wsToken);
                                }
                                // maybe another comment
                                comments.addAll(readComment(path, ret, wsToken));
                                return comments;
                            }
                            // if (!Character.isWhitespace(sb.subSequence(sb.length() - 1, sb.length()).charAt(0))) {
                            if (sb.length() > 0) {
                                sb.append("\r\n");
                                // }
                            }
                        } else {
                            sb.append(tmp);
                            sb.append(currentChar(path));
                            tmp = null;
                        }
                    } else {
                        sb.append(currentChar(path));
                    }
                }
            }
        }
        return comments;
    }

    /**
     * @param startindex
     *            TODO
     * @param endIndex
     *            TODO
     * @param path
     *            TODO
     * @param cleanComment
     * @param inline
     * @param unknown
     * @return
     */
    protected FlexiCommentJsonNode createComment(String comment, org.appwork.storage.flexijson.FlexiComment.Type type, FlexiMapperTags tag, int startindex, int endIndex, Object path) {
        return new FlexiComment(comment, type, tag);
    }

    protected FlexiJSonComments createCommentsContainer() {
        return new FlexiJSonComments();
    }

    /**
     * @param string
     * @return
     */
    protected String cleanComment(String string) {
        string = string.trim();
        string = string.replaceAll("\\s*[\r\n]{1,2}\\s*", "\r\n");
        return string;
    }

    /**
     * @return
     */
    public boolean isParseLineCommentEnabled() {
        return true;
    }

    /**
     * @return
     */
    public boolean isParseInlineCommentEnabled() {
        return true;
    }

    protected int readNextChar() throws IOException {
        if (aheadBuffer != null) {
            buffer[0] = aheadBuffer[0];
            aheadBuffer = null;
            return buffer[0] == END_OF_TRANSMISSION ? -1 : 1;
        }
        return reader.read(buffer);
    }

    private LinkedList<FlexiJSONParserExtension> tokenParserExtensions;

    public FlexiJSONParser addExtension(FlexiJSONParserExtension parser) {
        if (tokenParserExtensions == null) {
            tokenParserExtensions = new LinkedList<FlexiJSONParserExtension>();
        }
        tokenParserExtensions.add(parser);
        return this;
    }

    protected Object readToken(Object path, Token type) throws FlexiParserException {
        // string
        // try {
        isNumber.reset();
        isNull.reset();
        isTrue.reset();
        isFalse.reset();
        isUndefined.reset();
        isReference.reset();
        if (tokenParserExtensions != null) {
            for (FlexiJSONParserExtension sp : tokenParserExtensions) {
                sp.reset();
            }
        }
        sb.setLength(0);
        lastNonWhitespaceIndex = 0;
        boolean escaped = false;
        char c = currentChar(path);
        if (c == DOUBLE_QUOTE) {
            stringQuoting = StringQuoting.DOUBLE;
            if (!nextChar(path, ParsingError.EXPECTED_STRING_OR_DOUBLE_QUOTE)) {
                return assignFinalType(path, type);
            }
        } else if (c == SINGLE_QUOTE) {
            stringQuoting = StringQuoting.SINGLE;
            throwParserExceptionInternal(ParsingError.ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, path, null, container, null);
            if (!nextChar(path, ParsingError.EXPECTED_STRING_OR_SINGLE_QUOTE)) {
                // fallback for end of stream;
                return assignFinalType(path, type);
            }
        } else {
            stringQuoting = StringQuoting.NONE;
        }
        boolean inline = isParseInlineCommentEnabled();
        boolean line = isParseLineCommentEnabled();
        do {
            c = currentChar(path);
            if (stringQuoting == StringQuoting.NONE && c == SLASH) {
                // find comments in string without quotes
                if ((lookahead(path) == SLASH && line) || (lookahead(path) == ASTERISK && inline)) {
                    return assignFinalType(path, type);
                }
            }
            if (stringQuoting == StringQuoting.DOUBLE && c == DOUBLE_QUOTE) {
                try {
                    return assignFinalType(path, type);
                } finally {
                    nextChar(path, null);
                }
            } else if (stringQuoting == StringQuoting.SINGLE && c == SINGLE_QUOTE) {
                try {
                    return assignFinalType(path, type);
                } finally {
                    nextChar(path, null);
                }
            } else if (stringQuoting == StringQuoting.NONE) {
                if (container instanceof FlexiJSonObject) {
                    // these chars must be escaped if used in none-quoted strings
                    if (type == Token.KEY) {
                        switch (c) {
                        case COLON: // set correct token for the current character - since it already belongs to the next entry
                            setToken(path, Token.ASSIGN);
                            return assignFinalType(path, type);
                        case COMMA: // set correct token for the current character - since it already belongs to the next entry
                            setToken(path, Token.COMMA);
                            return assignFinalType(path, type);
                        case CURLY_BRACKET_CLOSE:
                            setToken(path, Token.END_OF_OBJECT);
                            return assignFinalType(path, type);
                        }
                    } else if (type == Token.VALUE) {
                        switch (c) {
                        case COMMA: // set correct token for the current character - since it already belongs to the next entry
                            setToken(path, Token.COMMA);
                            return assignFinalType(path, type);
                        case CURLY_BRACKET_CLOSE: // set correct token for the current character - since it already belongs to the next
                                                  // entry
                            if (isParseReferencesEnabled() && isReference.mayBeALink()) {
                                // this is the end of a reference
                            } else {
                                setToken(path, Token.END_OF_OBJECT);
                                return assignFinalType(path, type);
                            }
                        }
                    }
                } else if (container instanceof FlexiJSonArray) {
                    switch (c) {
                    case COMMA:
                        setToken(path, Token.COMMA);
                        return assignFinalType(path, type);
                    case SQUARE_BRACKET_CLOSE:
                        if (isParseReferencesEnabled() && isReference.isFinished()) {
                            // this is the end of a reference
                        } else {
                            setToken(path, Token.END_OF_ARRAY);
                        }
                        return assignFinalType(path, type);
                    }
                }
            } else if ((stringQuoting == StringQuoting.DOUBLE || stringQuoting == StringQuoting.SINGLE) && (c == CARRIAGE_RETURN || c == LINE_FEED)) {
                throwParserExceptionInternal(ParsingError.ERROR_NEWLINE_IN_TOKEN, path, null, container, null);
            }
            if (stringQuoting == StringQuoting.DOUBLE || stringQuoting == StringQuoting.SINGLE) {
                if (c == BACKSLASH) {
                    escaped = true;
                    if (!nextChar(path, ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER)) {
                        // if we ignore error, we add a backslash as failsafe and return the token
                        appendC(BACKSLASH);
                        return assignFinalType(path, type);
                    }
                    while ((c = currentChar(path)) == BACKSLASH) {
                        escaped = !escaped;
                        if (!escaped) {
                            appendC(c);
                            if (!nextChar(path, stringQuoting == StringQuoting.SINGLE ? ParsingError.EXPECTED_STRING_OR_SINGLE_QUOTE : ParsingError.EXPECTED_STRING_OR_DOUBLE_QUOTE)) {
                                appendC(BACKSLASH);
                                return assignFinalType(path, type);
                            }
                        } else {
                            if (!nextChar(path, ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER)) {
                                appendC(BACKSLASH);
                                return assignFinalType(path, type);
                            }
                        }
                    }
                    if (escaped) {
                        switch (c) {
                        case DOUBLE_QUOTE:
                        case SINGLE_QUOTE:
                        case SLASH:
                            break;
                        case 'r':
                            c = CARRIAGE_RETURN;
                            break;
                        case 'n':
                            c = LINE_FEED;
                            break;
                        case 't':
                            c = TAB;
                            break;
                        case 'f':
                            c = FORM_FEED;
                            break;
                        case 'b':
                            c = BACKSPACE;
                            break;
                        case 'u':
                            sb2.setLength(0);
                            // this.global++;
                            // counter = global + 4;
                            loop: for (int i = 0; i < 4; i++) {
                                if (!nextChar(path, ParsingError.EXPECTED_UNICODE_SEQUENCE)) {
                                    if (sb2.length() == 0) {
                                        c = (char) 0;
                                    } else {
                                        c = (char) Integer.parseInt(sb2.toString(), 16);
                                    }
                                    appendC(c);
                                    return assignFinalType(path, type);
                                }
                                c = currentChar(path);
                                if (!Character.isDigit(c)) {
                                    switch (c) {
                                    case 'a':
                                    case 'A':
                                    case 'b':
                                    case 'B':
                                    case 'c':
                                    case 'C':
                                    case 'd':
                                    case 'D':
                                    case 'e':
                                    case 'E':
                                    case 'f':
                                    case 'F':
                                        break;
                                    default:
                                        throwParserExceptionInternal(ParsingError.EXPECTED_UNICODE_SEQUENCE, path, null, container, null);
                                        break loop;
                                    }
                                }
                                if (sb2.length() > 0 || c != '0') {
                                    sb2.append(c);
                                }
                            }
                            // this.global--;
                            if (sb2.length() == 0) {
                                c = (char) 0;
                            } else {
                                c = (char) Integer.parseInt(sb2.toString(), 16);
                            }
                            break;
                        default:
                            throwParserExceptionInternal(ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER, path, null, container, null);
                            // JS ignores invalid - do not add a backslash here
                            // appendC(BACKSLASH);
                            continue;
                        }
                    } else {
                        // global--;
                        continue;
                    }
                }
            } else {
                if (c == BACKSLASH) {
                    escaped = true;
                    if (!nextChar(path, ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER)) {
                        // if we ignore error, we add a backslash as failsafe and return the token
                        appendC(BACKSLASH);
                        return assignFinalType(path, type);
                    }
                    while ((c = currentChar(path)) == BACKSLASH) {
                        escaped = !escaped;
                        if (!escaped) {
                            appendC(c);
                            if (!nextChar(path, null)) {
                                // "\\\\"
                                return assignFinalType(path, type);
                            }
                        } else {
                            if (!nextChar(path, ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER)) {
                                appendC(BACKSLASH);
                                return assignFinalType(path, type);
                            }
                        }
                    }
                    if (escaped) {
                        switch (c) {
                        case DOUBLE_QUOTE:
                        case SINGLE_QUOTE:
                        case COMMA:
                            // case '{':
                            // case '[':
                        case ']':
                        case CURLY_BRACKET_CLOSE:
                            break;
                        case COLON:
                            if (type != Token.KEY) {
                                throwParserExceptionInternal(ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER, path, null, container, null);
                            }
                            break;
                        case 'u':
                            sb2.setLength(0);
                            // this.global++;
                            // counter = global + 4;
                            loop: for (int i = 0; i < 4; i++) {
                                if (!nextChar(path, ParsingError.EXPECTED_UNICODE_SEQUENCE)) {
                                    if (sb2.length() == 0) {
                                        c = (char) 0;
                                    } else {
                                        c = (char) Integer.parseInt(sb2.toString(), 16);
                                    }
                                    appendC(c);
                                    return assignFinalType(path, type);
                                }
                                c = currentChar(path);
                                if (!Character.isDigit(c)) {
                                    switch (c) {
                                    case 'a':
                                    case 'A':
                                    case 'b':
                                    case 'B':
                                    case 'c':
                                    case 'C':
                                    case 'd':
                                    case 'D':
                                    case 'e':
                                    case 'E':
                                        break;
                                    default:
                                        throwParserExceptionInternal(ParsingError.EXPECTED_UNICODE_SEQUENCE, path, null, container, null);
                                        break loop;
                                    }
                                }
                                if (sb2.length() > 0 || c != '0') {
                                    sb2.append(c);
                                }
                            }
                            // this.global--;
                            if (sb2.length() == 0) {
                                c = (char) 0;
                            } else {
                                c = (char) Integer.parseInt(sb2.toString(), 16);
                            }
                            break;
                        default:
                            throwParserExceptionInternal(ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER, path, null, container, null);
                            appendC(BACKSLASH);
                            continue;
                        }
                    } else {
                        // global--;
                        continue;
                    }
                }
            }
            appendC(c);
            switch (stringQuoting) {
            case SINGLE:
                if (!nextChar(path, ParsingError.EXPECTED_STRING_OR_SINGLE_QUOTE)) {
                    return assignFinalType(path, type);
                }
                break;
            case DOUBLE:
                if (!nextChar(path, ParsingError.EXPECTED_STRING_OR_DOUBLE_QUOTE)) {
                    return assignFinalType(path, type);
                }
                break;
            default:
                // not reached the end of the token -> continue;
                if (!nextChar(path, null)) {
                    return assignFinalType(path, type);
                }
            }
        } while (true);
    }

    public FlexiJSONParser setDebug(StringBuilder debug) {
        this.debug = debug;
        return this;
    }

    /**
     * Set a list of ParsingErrors that should not throw an expection. The parser will try to parse the json anyway
     *
     * @param ignoreIssues
     * @return
     */
    public FlexiJSONParser setIgnoreIssues(Object... issues) {
        this.ignoreIssues = new HashSet<ParsingError>();
        return addIgnoreIssues(issues);
    }

    public FlexiJSONParser addIgnoreIssues(Object... issues) {
        for (Object i : issues) {
            if (i instanceof ParsingError) {
                ignoreIssues.add((ParsingError) i);
            } else if (i instanceof Set) {
                ignoreIssues.addAll((Set<ParsingError>) i);
            } else {
                throw new IllegalArgumentException("Type " + CompiledType.create(i.getClass()) + " is not supported");
            }
        }
        return this;
    }

    /**
     *
     */
    public FlexiJSONParser setIgnoreIssues(ParsingError... ignoreIssues) {
        return setIgnoreIssues(new HashSet<ParsingError>(Arrays.asList(ignoreIssues)));
    }

    public void setToken(Object path, Token token) throws FlexiParserException {
        this.token = token;
    }

    protected boolean skipWhitespace(Object path, ParsingError expectedContentAfterWhitespace, Token token) throws FlexiParserException {
        do {
            if (expectedContentAfterWhitespace != null && isEndOfStreamReached()) {
                throwParserExceptionInternal(expectedContentAfterWhitespace, path, null, container, null);
            }
            if (Character.isWhitespace(currentChar(path))) {
                if (token != null) {
                    setToken(path, token);
                    token = null;
                }
            } else {
                break;
            }
            if (!nextChar(path, expectedContentAfterWhitespace)) {
                return false;
            }
        } while (true);
        return buffer[0] != END_OF_TRANSMISSION;
    }

    protected boolean skipWhitespaceToChar(Object path, ParsingError expectedContentAfterWhitespace, Token token, char nextNonWhitespace) throws FlexiParserException {
        skipWhitespace(path, expectedContentAfterWhitespace, token);
        if (currentChar(path) != nextNonWhitespace) {
            throwParserExceptionInternal(expectedContentAfterWhitespace, path, null, container, null);
        }
        return buffer[0] == nextNonWhitespace;
    }

    private void throwParserExceptionInternal(ParsingError error, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
        throwParserException(error, path, cause, parent, value);
    }

    protected void throwParserException(ParsingError error, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
        if (getIgnoreIssues().contains(error)) {
            return;
        } else {
            String json = "";
            if (debug != null) {
                json = "\r\n[...]" + debug.subSequence(Math.max(0, index - 100), Math.min(debug.length(), index + 20)) + " ";
                json += "\r\n" + StringUtils.fillPost("", " ", Math.min(index, 100) + 4) + "^\r\n";
            }
            json += error.name() + ": " + error.description + " at index " + index;
            throw new FlexiParserException(error, index, path, json, cause);
        }
    }

    @Override
    public String toString() {
        if (debug != null) {
            return debug.toString() + "| " + (isEndOfStreamReached() ? "<END>" : ("'" + currentChar(null) + "'"));
        } else {
            return "" + (isEndOfStreamReached() ? "<END>" : ("'" + currentChar(null) + "'"));
        }
    }

    /**
     * @return
     */
    public FlexiJSONParser debug() {
        this.debug = new StringBuilder();
        return this;
    }

    /**
     * @param ignoreListEnsureCorrectValues
     * @return
     */
    public FlexiJSONParser ignoreIssues(HashSet<ParsingError> ignoreListEnsureCorrectValues) {
        setIgnoreIssues(ignoreListEnsureCorrectValues);
        return this;
    }

    public FlexiJSONParser breakAtEndOfObject() {
        setBreakAtEndOfObject(true);
        return this;
    }

    private boolean breakAtEndOfObject = false;

    public boolean isBreakAtEndOfObject() {
        return breakAtEndOfObject;
    }

    public void setBreakAtEndOfObject(boolean breakAtEndOfObject) {
        this.breakAtEndOfObject = breakAtEndOfObject;
    }

    /**
     * @return
     */
    public FlexiJSONParser enableComments() {
        return addIgnoreIssues(FlexiJSONParser.IGNORE_LIST_COMMENTS);
    }

    public FlexiJSONParser enableExtraCommas() {
        return addIgnoreIssues(FlexiJSONParser.IGNORE_LIST_EXTRA_COMMAS);
    }

    /**
     * @return
     */
    public FlexiJSONParser enableUndefinedValues() {
        return addIgnoreIssues(ParsingError.ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED);
    }

    public FlexiJSONParser enableNaNValues() {
        return addIgnoreIssues(ParsingError.ERROR_NUMBER_NAN);
    }

    public FlexiJSONParser enableInfinityValues() {
        return addIgnoreIssues(ParsingError.ERROR_NUMBER_INFINITY);
    }
}
