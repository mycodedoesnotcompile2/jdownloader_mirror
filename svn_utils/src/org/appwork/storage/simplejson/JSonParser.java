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
package org.appwork.storage.simplejson;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.appwork.utils.CharSequenceUtils;

/**
 * @author thomas
 */
public class JSonParser {
    /**
     *
     */
    private static final char[] CHAR_ARRAY_FALSE     = "false".toCharArray();
    /**
     *
     */
    private static final char[] CHAR_ARRAY_TRUE      = "true".toCharArray();
    /**
     *
     */
    private static final char[] CHAR_ARRAY_NULL      = "null".toCharArray();
    private static final char[] CHAR_ARRAY_UNDEFINED = "undefined".toCharArray();
    public static boolean       DEBUG                = false;
    protected int               global               = 0;
    public final String         str;
    final StringBuilder         sb;
    private final StringBuilder sb2;
    private int                 counter;
    private String              debug;
    protected final boolean     naNAllowed;
    private Token               token;

    public JSonParser(final String json) {
        this(json, false);
    }

    public JSonParser(final String json, boolean naNAllowed) {
        str = wrap(this, json);
        sb = new StringBuilder();
        sb2 = new StringBuilder();
        counter = 0;
        this.naNAllowed = naNAllowed;
    }

    private static String wrap(final JSonParser parser, final String string) {
        if (string == null || string.length() == 0) {
            return null;
        }
        final char bomCheck = string.charAt(0);
        if (bomCheck == 0xfeff) {
            // UTF-16BE
            return string.substring(1);
        } else if (bomCheck == 0xfffe) {
            // UTF-16LE
            return string.substring(1);
        } else {
            return string;
        }
    }

    protected ParserException bam(final String expected, Object path) {
        return bam(expected, path, null);
    }

    protected ParserException bam(final String expected, final Object path, final Throwable cause) {
        final int lastX = Math.min(str.length(), 20);
        String pre = str.substring(Math.max(global - lastX, 0), lastX);
        pre = pre.replace("\r", "\\r").replace("\n", "\\n");
        final StringBuilder sb = new StringBuilder();
        sb.append(expected);
        sb.append("\r\n\t");
        sb.append(pre);
        sb.append(str.substring(Math.min(str.length(), global), Math.min(str.length(), global + lastX)));
        sb.append("\r\n\t");
        for (int i = 1; i < pre.length(); i++) {
            sb.append("-");
        }
        sb.append('|');
        return new ParserException(global, path, sb.toString(), cause);
    }

    protected String findString(Object path) throws ParserException {
        // string
        try {
            sb.setLength(0);
            char c = charAt(path, global++);
            if (c != '\"') {
                throwParserException("'\"' expected", path);
                return null;
            }
            boolean escaped = false;
            while (true) {
                c = charAt(path, global++);
                switch (c) {
                case '\"':
                    return sb.toString();
                case '\\':
                    escaped = true;
                    while ((c = charAt(path, global++)) == '\\') {
                        escaped = !escaped;
                        if (!escaped) {
                            sb.append("\\");
                        }
                    }
                    if (escaped) {
                        switch (c) {
                        case '"':
                        case '/':
                            sb.append(c);
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
                            sb2.setLength(0);
                            // this.global++;
                            counter = global + 4;
                            for (; global < counter; global++) {
                                c = getChar(path);
                                if (sb2.length() > 0 || c != '0') {
                                    sb2.append(c);
                                }
                            }
                            // this.global--;
                            if (sb2.length() == 0) {
                                sb.append((char) 0);
                            } else {
                                sb.append((char) Integer.parseInt(sb2.toString(), 16));
                            }
                            continue;
                        default:
                            throwParserException("illegal escape char", path);
                            return sb.toString();
                        }
                    } else {
                        global--;
                    }
                    break;
                default:
                    sb.append(c);
                }
            }
        } catch (final StringIndexOutOfBoundsException e) {
            global--;
            throwParserException("Unexpected End of String \"" + sb.toString(), path, e);
            return sb.toString();
        }
    }

    protected char getChar(Object path) throws ParserException {
        if (isDebugEnabled()) {
            final String pos = str.substring(0, global);
            debug = pos + str.substring(global) + "\r\n";
            for (int i = 0; i < pos.length(); i++) {
                debug += "-";
            }
            debug += '\u2934';
            System.err.println(debug);
        }
        try {
            return charAt(path, global);
        } catch (IndexOutOfBoundsException e) {
            throw throwParserException("Ended unexpected", path, e);
        }
    }

    /**
     * @param path
     * @param global2
     * @return
     */
    protected char charAt(Object path, int index) {
        return str.charAt(index);
    }

    /**
     * @return
     */
    protected boolean isDebugEnabled() {
        return DEBUG;
    }

    public Object parse() throws ParserException {
        final Object ret = parseValue(null);
        skipWhiteSpace(null);
        if (global != str.length()) {
            skipWhiteSpace(null, true);// ignore NUL at the end of the JSON string
            if (global != str.length()) {
                global++;
                throwParserException("Unexpected End of JSonString", "");
            }
        }
        return ret;
    }

    protected List<? extends Object> parseArray(Object path) throws ParserException {
        setToken(path, Token.START_OF_ARRAY);
        global++;
        final List<? extends Object> ret = createJSonArray();
        while (true) {
            // skip whitespace
            parseArrayValue((List<Object>) ret, path);
            final char c = getChar(path);
            switch (c) {
            case ',':
                setToken(path, Token.COMMA);
                // ok another round:
                global++;
                break;
            case ']':
                setToken(path, Token.END_OF_ARRAY);
                // end
                global++;
                return ret;
            default:
                throwParserException("']' or ',' expected", path);
            }
        }
    }

    protected Object parseArrayValue(final List<Object> ret, Object path) throws ParserException {
        setToken(path, Token.WS_BEFORE_VALUE);
        skipWhiteSpace(path);
        final char c = getChar(path);
        Object newPath = path;
        switch (c) {
        case ']':
            break;
        case ',':
            throwParserException("Value missing", path);
        default:
            ret.add(parseValue(newPath = extendPath(path, ret.size())));
            setToken(path, Token.WS_AFTER_VALUE);
            skipWhiteSpace(path);
        }
        return newPath;
    }

    protected List<? extends Object> createJSonArray() {
        return new ArrayList<Object>();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return global + ". " + str.charAt(global);
    }

    protected static enum NumberFormat {
        FIXED,
        EXPLICIT_HEX,
        EXPLICIT_BINARY,
        EXPLICIT_OCTAL,
        FLOAT,
        NaN,
        FLOAT_EXP
    }

    protected boolean isValidNumberCharacter(final NumberFormat numberFormat, final CharSequence numberCharSequence, char nextChar) {
        if (numberFormat == null) {
            if (getNumberFormat(null, numberCharSequence, nextChar) != null) {
                // switch to NumberFormat
                return true;
            } else if (nextChar >= '0' && nextChar <= '9') {
                return true;
            } else if (nextChar == '+' || nextChar == '-') {
                return true;
            } else {
                return false;
            }
        } else {
            switch (numberFormat) {
            case NaN:
                return (numberCharSequence.length() == 1 && nextChar == 'a') || (numberCharSequence.length() == 2 && nextChar == 'N');
            case EXPLICIT_BINARY:
                return nextChar == '0' || nextChar == '1';
            case EXPLICIT_OCTAL:
                return nextChar >= '0' && nextChar <= '7';
            case EXPLICIT_HEX:
                if (nextChar >= 'a' && nextChar <= 'f') {
                    return true;
                } else if (nextChar >= 'A' && nextChar <= 'F') {
                    return true;
                } else if (nextChar >= '0' && nextChar <= '9') {
                    return true;
                } else {
                    return false;
                }
            case FLOAT:
                if (nextChar == 'e' || nextChar == 'E') {
                    return true;
                } else if (nextChar >= '0' && nextChar <= '9') {
                    return true;
                } else {
                    return false;
                }
            case FLOAT_EXP:
                if (nextChar == '+' || nextChar == '-') {
                    return true;
                } else {
                    if (nextChar >= '0' && nextChar <= '9') {
                        return true;
                    } else {
                        return false;
                    }
                }
            default:
                return false;
            }
        }
    }

    protected NumberFormat getNumberFormat(NumberFormat numberFormat, CharSequence numberCharSequence, char nextChar) {
        if (numberFormat != null) {
            switch (numberFormat) {
            case FLOAT:
                if (nextChar == 'e' || nextChar == 'E') {
                    return NumberFormat.FLOAT_EXP;
                }
                break;
            default:
                break;
            }
            return numberFormat;
        }
        if (nextChar == '.') {
            return NumberFormat.FLOAT;
        }
        if ((nextChar == 'e' || nextChar == 'E') && numberCharSequence.length() > 0) {
            final char last = numberCharSequence.charAt(numberCharSequence.length() - 1);
            if (Character.isDigit(last)) {
                return NumberFormat.FLOAT_EXP;
            }
        }
        if (CharSequenceUtils.endsWith(numberCharSequence, "0")) {
            if (nextChar == 'x') {
                return NumberFormat.EXPLICIT_HEX;
            } else if (nextChar == 'b') {
                return NumberFormat.EXPLICIT_BINARY;
            } else if (nextChar == 'o') {
                return NumberFormat.EXPLICIT_OCTAL;
            }
        }
        if (isNaNAllowed() && nextChar == 'N' && numberCharSequence.length() == 0) {
            return NumberFormat.NaN;
        }
        return null;
    }

    protected boolean isNaNAllowed() {
        return naNAllowed;
    }

    protected boolean isImplicitOctalAllowed() {
        return false;
    }

    protected boolean isBigIntegerAllowed() {
        return false;
    }

    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates
    protected Object parseNumber(Object path) throws ParserException, NoNumberException {
        sb.setLength(0);
        NumberFormat numberFormat = null;
        char c = getChar(path);
        if (isValidNumberCharacter(numberFormat, sb, c)) {
            numberFormat = getNumberFormat(numberFormat, sb, c);
            sb.append(c);
            while (global + 1 < str.length()) {
                global++;
                c = getChar(path);
                if (isValidNumberCharacter(numberFormat, sb, c)) {
                    numberFormat = getNumberFormat(numberFormat, sb, c);
                    sb.append(c);
                } else {
                    global--;
                    break;
                }
            }
            global++;
            if (NumberFormat.NaN.equals(numberFormat)) {
                return createJSonValue(Double.NaN);
            } else if (NumberFormat.FLOAT.equals(numberFormat) || NumberFormat.FLOAT_EXP.equals(numberFormat)) {
                return createJSonValue(parseFloatNumber(sb));
            } else {
                return createJSonValue(parseFixedNumber(sb));
            }
        } else {
            throw new NoNumberException();
        }
    }

    protected Number parseFloatNumber(final CharSequence charSequence) {
        final String string = charSequence.toString();
        final double num = Double.parseDouble(string);
        // doesn't throw NumberFormatException when no longer *fit* into double but simply looses precision
        final Number ret;
        final float check = (float) num;
        if (check == num && Float.toString(check).length() >= string.length()) {
            ret = Float.valueOf(check);
        } else {
            ret = Double.valueOf(num);
        }
        return ret;
    }

    protected Number parseFixedNumber(final CharSequence charSequence) {
        String string = charSequence.toString();
        int radix = 10;// decimal
        if (string.matches("^(-|\\+)?0(x|X).+")) {
            radix = 16;// explicit hex
            string = string.replaceFirst("0(x|X)", "");
        } else if (string.matches("^(-|\\+)?0(o|O).+")) {
            radix = 8;// explicit octal
            string = string.replaceFirst("0(o|O)", "");
        } else if (string.matches("^(-|\\+)?0(b|B).+")) {
            radix = 2;// explicit binary
            string = string.replaceFirst("0(b|B)", "");
        } else if (string.matches("^(-|\\+)?0[0-7]+") && isImplicitOctalAllowed()) {
            radix = 8;// implicit octal
        }
        final Long ret;
        try {
            ret = Long.valueOf(string, radix);
        } catch (NumberFormatException e) {
            // is thrown when number doesn't fit into Long
            if (isBigIntegerAllowed()) {
                return new BigInteger(string, radix);
            } else {
                throw e;
            }
        }
        if (ret.longValue() <= Integer.MAX_VALUE && ret.longValue() >= Integer.MIN_VALUE) {
            if (ret.longValue() <= Short.MAX_VALUE && ret.longValue() >= Short.MIN_VALUE) {
                if (ret.longValue() <= Byte.MAX_VALUE && ret.longValue() >= Byte.MIN_VALUE) {
                    return Byte.valueOf(ret.byteValue());
                }
                return Short.valueOf(ret.shortValue());
            }
            return Integer.valueOf(ret.intValue());
        }
        return ret;
    }

    protected Object createJSonValue(Number number) {
        return number;
    }

    /**
     * Warning... tokens are sorted based on their relevance. Do not change
     *
     * @author thomas
     * @date 18.03.2021
     *
     */
    public static enum Token {
        KEY,
        VALUE,
        VALUE_STRING,
        VALUE_NULL,
        VALUE_UNDEFINED,
        VALUE_BOOLEAN,
        VALUE_NUMBER,
        END_OF_OBJECT,
        START_OF_OBJECT,
        START_OF_ARRAY,
        END_OF_ARRAY,
        ASSIGN,
        COMMA,
        WS_BEFORE_VALUE,
        WS_AFTER_VALUE,
        WS_AFTER_ASSIGN,
        WS_AFTER_KEY,
        WS_BEFORE_KEY
    }

    protected Map<String, ? extends Object> parseObject(Object path) throws ParserException {
        setToken(path, Token.START_OF_OBJECT);
        global++;
        final Map<String, ? extends Object> ret = createJSonObject();
        while (true) {
            // check for object end markers
            parseKeyValuePair(path, (Map<String, Object>) ret);
            if (global >= str.length()) {
                throwParserException("} or , expected", path);
            }
            final char c = getChar(path);
            switch (c) {
            case ',':
                setToken(path, Token.COMMA);
                // ok another value...probably
                global++;
                break;
            case '}':
                // end of object:
                setToken(path, Token.END_OF_OBJECT);
                global++;
                // create new JSonObject with existing JSonObject, Map implementation likely will use optimized/smaller internal table
                return ret;
            default:
                throwParserException(", or }' expected", path);
                global++;
            }
        }
    }

    /**
     * @param string
     * @param path
     * @param e
     * @throws ParserException
     */
    protected ParserException throwParserException(String string, Object path) throws ParserException {
        ParserException ex = bam(string, path);
        if (ex != null) {
            throw ex;
        }
        return ex;
    }

    protected ParserException throwParserException(String string, Object path, Throwable e) throws ParserException {
        ParserException ex = bam(string, path, e);
        if (ex != null) {
            throw ex;
        }
        return ex;
    }

    protected Object parseKeyValuePair(Object newPath, final Map<String, Object> ret) throws ParserException {
        setToken(newPath, Token.WS_BEFORE_KEY);
        skipWhiteSpace(newPath);
        char c = getChar(newPath);
        if (c == '"') {
            setToken(newPath, Token.KEY);
            String key = findString(newPath);
            key = dedupeString(key);
            setToken(newPath, Token.WS_AFTER_KEY);
            newPath = extendPath(newPath, key);
            skipWhiteSpace(newPath);
            setToken(newPath, Token.ASSIGN);
            c = getChar(newPath);
            if (c != ':') {
                throwParserException("':' expected", newPath);
            }
            global++;
            setToken(newPath, Token.WS_AFTER_ASSIGN);
            skipWhiteSpace(newPath);
            setToken(newPath, Token.VALUE);
            Object newValue = parseValue(newPath);
            final String mappedKey = mapKey(key);
            if (ret.containsKey(mappedKey)) {
                onDuplicateProperty(newPath, ret, newValue);
            }
            putKeyValuePair(newPath, ret, mappedKey, newValue);
            setToken(newPath, Token.WS_AFTER_VALUE);
            skipWhiteSpace(newPath);
        }
        return newPath;
    }

    protected void putKeyValuePair(Object newPath, final Map<String, Object> map, String key, Object value) {
        map.put(key, value);
    }

    /**
     * override to throw errors if the same key is used multiple times
     *
     * @param key
     * @param ret
     * @param newValue
     */
    protected void onDuplicateProperty(Object path, Object parent, Object newValue) {
    }

    protected Map<String, ? extends Object> createJSonObject() {
        return new LinkedHashMap<String, Object>();
    }

    public Token getToken() {
        return token;
    }

    public void setToken(Object path, Token token) throws ParserException {
        this.token = token;
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

    /**
     * @param key
     * @return
     */
    protected String mapKey(String key) {
        return key;
    }

    protected Object parseValue(Object path) throws ParserException {
        setToken(path, Token.WS_BEFORE_VALUE);
        global = skipWhiteSpace(path, global == 0);// ignore NUL at the start of the string
        setToken(path, Token.VALUE);
        switch (getChar(path)) {
        case '{':
            return parseObject(path);
        case '[':
            return parseArray(path);
        case 'n':
            setToken(path, Token.VALUE_NULL);
            expectChars(path, CHAR_ARRAY_NULL);
            return createJSonValue((String) null);
        case 'u':
            setToken(path, Token.VALUE_UNDEFINED);
            expectChars(path, CHAR_ARRAY_UNDEFINED);
            return createJSonValue((String) null);
        case 't':
            setToken(path, Token.VALUE_BOOLEAN);
            expectChars(path, CHAR_ARRAY_TRUE);
            return createJSonValue(true);
        case 'f':
            // false;
            setToken(path, Token.VALUE_BOOLEAN);
            expectChars(path, CHAR_ARRAY_FALSE);
            return createJSonValue(false);
        case '"':
            setToken(path, Token.VALUE_STRING);
            final String string = findString(path);
            return createJSonValue(string);
        }
        try {
            setToken(path, Token.VALUE_NUMBER);
            return parseNumber(path);
        } catch (final NumberFormatException e) {
            global++;
            throwParserException("Illegal Char", path, e);
            // for parsing all errors, we can override throwParserException to not throw an exception
            return parseValue(path);
        } catch (final NoNumberException e) {
            global++;
            throwParserException("Illegal Char", path, e);
            // for parsing all errors, we can override throwParserException to not throw an exception
            return parseValue(path);
        }
    }

    private final HashMap<String, String> DEDUPEMAP = new HashMap<String, String>();

    protected Map<String, String> getDedupeMap() {
        return DEDUPEMAP;
    }

    protected String dedupeString(final String string) {
        final Map<String, String> dedupeMap = getDedupeMap();
        if (string != null && dedupeMap != null) {
            synchronized (dedupeMap) {
                String deduped = dedupeMap.get(string);
                if (deduped == null) {
                    dedupeMap.put(string, string);
                    deduped = string;
                }
                return deduped;
            }
        } else {
            return string;
        }
    }

    /**
     * @param object
     * @return
     */
    protected Object createJSonValue(String value) {
        return value;
    }

    /**
     * @param b
     * @return
     */
    protected Object createJSonValue(boolean b) {
        return b ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * @param path
     * @param charArray
     */
    protected void expectChars(Object path, char[] chars) throws ParserException {
        for (char c : chars) {
            final ParserException parserException = expectChar(path, c);
            if (parserException != null) {
                throw parserException;
            }
        }
    }

    /**
     * @param d
     */
    protected ParserException expectChar(Object path, char expected) {
        final char is = charAt(path, global++);
        if (is != expected) {
            return bam("Unexpected char: " + is + " instead of " + expected, path);
        } else {
            return null;
        }
    }

    protected int skipWhiteSpace(Object path) throws ParserException {
        return skipWhiteSpace(path, false);
    }

    protected int skipWhiteSpace(Object path, final boolean skipNull) throws ParserException {
        while (global < str.length()) {
            final char c = charAt(path, global++);
            if (!Character.isWhitespace(c) && (!skipNull || Character.MIN_VALUE != c)) {
                global--;
                break;
            }
        }
        return global;
    }

    /**
     * @param jsonString
     * @return
     * @throws ParserException
     */
    public static String decodeJavaScriptString(String str) throws ParserException {
        if (str == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        StringBuilder sb2 = new StringBuilder();
        boolean escaped = false;
        int global = 0;
        int counter = 0;
        char c = str.charAt(global++);
        if (c != '\"') {
            throw new ParserException(global, null, "'\"' expected");
        }
        sb.append("\"");
        while (global < str.length()) {
            c = str.charAt(global++);
            switch (c) {
            case '\"':
                sb.append("\"");
                return sb.toString();
            case '\\':
                escaped = true;
                while ((c = str.charAt(global++)) == '\\') {
                    escaped = !escaped;
                    if (!escaped) {
                        sb.append("\\");
                    }
                }
                if (escaped) {
                    switch (c) {
                    // case '"':
                    // case '/':
                    // sb.append(c);
                    // continue;
                    // case 'r':
                    // sb.append('\r');
                    // continue;
                    // case 'n':
                    // sb.append('\n');
                    // continue;
                    // case 't':
                    // sb.append('\t');
                    // continue;
                    // case 'f':
                    // sb.append('\f');
                    // continue;
                    // case 'b':
                    // sb.append('\b');
                    // continue;
                    case 'x':
                        sb2.setLength(0);
                        // this.global++;
                        counter = global + 2;
                        for (; global < counter; global++) {
                            c = str.charAt(global);
                            if (sb2.length() > 0 || c != '0') {
                                sb2.append(c);
                            }
                        }
                        // this.global--;
                        if (sb2.length() == 0) {
                            sb.append((char) 0);
                        } else {
                            sb.append("\\").append((char) Short.parseShort(sb2.toString(), 16));
                        }
                        continue;
                    default:
                    }
                } else {
                    global--;
                }
                break;
            default:
                sb.append(c);
            }
        }
        throw new ParserException(global, null, "Unfinished String");
    }
}
