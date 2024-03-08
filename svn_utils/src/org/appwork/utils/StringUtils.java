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
package org.appwork.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtils {
    public static boolean contains(final String input, final String contains) {
        if (input == null || contains == null) {
            return false;
        } else {
            return input.contains(contains);
        }
    }

    /**
     * WARNING: calls String.trim on each line!
     *
     * @param arg
     * @return
     */
    public static String[] getLines(final String arg) {
        if (arg == null) {
            return new String[] {};
        } else {
            final String[] splits = arg.split("(\r\n|\r|\n)");
            final ArrayList<String> ret = new ArrayList<String>(splits.length);
            for (final String split : splits) {
                ret.add(split.trim());
            }
            return ret.toArray(new String[0]);
        }
    }

    /**
     * Returns formatted number as String according to given padLength. </br> E.g. number 1 with padLength 3 will return "001".
     */
    public static String formatByPadLength(final int padLength, final int num) {
        return String.format(Locale.US, "%0" + padLength + "d", num);
    }

    /**
     * @param name
     * @param jdPkgRule
     * @return
     */
    public static boolean endsWithCaseInsensitive(final String name, final String endsWith) {
        if (name == null || endsWith == null) {
            return false;
        } else if (endsWith.length() > name.length()) {
            return false;
        } else if (StringUtils.isEmpty(name) || StringUtils.isEmpty(endsWith)) {
            return false;
        } else {
            return name.endsWith(endsWith) || name.toLowerCase(Locale.ENGLISH).endsWith(endsWith.toLowerCase(Locale.ENGLISH));
        }
    }

    public static boolean startsWithCaseInsensitive(final String name, final String startsWith) {
        if (name == null || startsWith == null) {
            return false;
        } else if (startsWith.length() > name.length()) {
            return false;
        } else if (StringUtils.isEmpty(name) || StringUtils.isEmpty(startsWith)) {
            return false;
        } else {
            return name.startsWith(startsWith) || name.toLowerCase(Locale.ENGLISH).startsWith(startsWith.toLowerCase(Locale.ENGLISH));
        }
    }

    public static boolean containsIgnoreCase(String input, String contains) {
        if (input == null || contains == null) {
            return false;
        } else if (contains.length() > input.length()) {
            return false;
        } else {
            return input.contains(contains) || input.toLowerCase(Locale.ENGLISH).contains(contains.toLowerCase(Locale.ENGLISH));
        }
    }

    /**
     * taken from http://stackoverflow.com/questions/4731055/whitespace-matching-regex-java
     */
    final private static String whitespace_chars = "[" /*
                                                        * dummy empty string for homogeneity
                                                        */
                                                         + "\\u0009" // CHARACTER
                                                         // TABULATION
                                                         + "\\u000A" // LINE
                                                         // FEED
                                                         // (LF)
                                                         + "\\u000B" // LINE
                                                         // TABULATION
                                                         + "\\u000C" // FORM
                                                         // FEED
                                                         // (FF)
                                                         + "\\u000D" // CARRIAGE
                                                         // RETURN
                                                         // (CR)
                                                         + "\\u0020" // SPACE
                                                         + "\\u0085" // NEXT
                                                         // LINE
                                                         // (NEL)
                                                         + "\\u00A0" // NO-BREAK
                                                         // SPACE
                                                         + "\\u1680" // OGHAM
                                                         // SPACE
                                                         // MARK
                                                         + "\\u180E" // MONGOLIAN
                                                         // VOWEL
                                                         // SEPARATOR
                                                         + "\\u2000" // EN QUAD
                                                         + "\\u2001" // EM QUAD
                                                         + "\\u2002" // EN SPACE
                                                         + "\\u2003" // EM SPACE
                                                         + "\\u2004" // THREE-PER-EM
                                                         // SPACE
                                                         + "\\u2005" // FOUR-PER-EM
                                                         // SPACE
                                                         + "\\u2006" // SIX-PER-EM
                                                         // SPACE
                                                         + "\\u2007" // FIGURE
                                                         // SPACE
                                                         + "\\u2008" // PUNCTUATION
                                                         // SPACE
                                                         + "\\u2009" // THIN
                                                         // SPACE
                                                         + "\\u200A" // HAIR
                                                         // SPACE
                                                         + "\\u2028" // LINE
                                                         // SEPARATOR
                                                         + "\\u2029" // PARAGRAPH
                                                         // SEPARATOR
                                                         + "\\u202F" // NARROW
                                                         // NO-BREAK
                                                         // SPACE
                                                         + "\\u205F" // MEDIUM
                                                         // MATHEMATICAL
                                                         // SPACE
                                                         + "\\u3000" // IDEOGRAPHIC
                                                         // SPACE
                                                         + "]";

    public static String trim(String input) {
        if (input != null) {
            return removeBOM(input.replaceAll("^" + StringUtils.whitespace_chars + "+", "").replaceAll(StringUtils.whitespace_chars + "+$", ""));
        } else {
            return null;
        }
    }

    public static String removeBOM(final String input) {
        // Strings in Java are UTF-16 BE, BOM
        if (input != null && input.startsWith("\uFEFF")) {
            return input.substring(1);
        } else {
            return input;
        }
    }

    public static String nullOrNonEmpty(String x) {
        if (StringUtils.isNotEmpty(x)) {
            return x;
        } else {
            return null;
        }
    }

    public static String valueOrEmpty(String x) {
        if (x == null) {
            return "";
        } else {
            return x;
        }
    }

    public static String nullify(String string) {
        if (string == null || "null".equals(string)) {
            return null;
        } else {
            return string;
        }
    }

    public static boolean equals(final String x, final String y) {
        if (x == y) {
            return true;
        } else if (x == null && y != null) {
            return false;
        } else if (y == null && x != null) {
            return false;
        } else {
            return x.equals(y);
        }
    }

    /**
     * @param x
     * @param y
     * @return
     */
    public static boolean equals(final String... values) {
        final int len = values.length;
        final String first = values[0];
        for (int index = 1; index < len; index++) {
            if (!equals(first, values[index])) {
                return false;
            }
        }
        return true;
    }

    public static boolean equalsIgnoreCase(final String x, final String y) {
        if (x == y) {
            return true;
        } else {
            return x != null && x.equalsIgnoreCase(y);
        }
    }

    /**
     * @param pass
     * @param pass2
     * @return
     */
    public static boolean equalsIgnoreCase(final String... values) {
        final int len = values.length;
        final String first = values[0];
        for (int index = 1; index < len; index++) {
            if (!equalsIgnoreCase(first, values[index])) {
                return false;
            }
        }
        return true;
    }

    private static String EMPTY_SPACE_STRING = "                                                                                                                                                                                                                                                             ";

    public static String fillPre(final String string, final String filler, final int minCount) {
        if (string.length() >= minCount) {
            return string;
        }
        final StringBuilder sb = new StringBuilder(minCount);
        int missing = minCount - string.length();
        // the EMPTY_SPACE_STRING approach was 30% faster in my usercase (SimpleFormater(
        if (" ".equals(filler) && missing <= EMPTY_SPACE_STRING.length()) {
            sb.append(EMPTY_SPACE_STRING, 0, missing);
            sb.append(string);
        } else {
            missing = missing / filler.length() + (missing % filler.length() == 0 ? 0 : 1);
            for (int i = 0; i < missing; i++) {
                sb.append(filler);
            }
            sb.append(string);
        }
        return sb.toString();
    }

    public static String fillPost(final String string, final String filler, final int minCount) {
        if (string.length() >= minCount) {
            return string;
        }
        final StringBuilder sb = new StringBuilder(minCount);
        int missing = minCount - string.length();
        if (" ".equals(filler) && missing <= EMPTY_SPACE_STRING.length()) {
            sb.append(string);
            sb.append(EMPTY_SPACE_STRING, 0, missing);
            return sb.toString();
        } else {
            sb.append(string);
            while (sb.length() < minCount) {
                sb.append(filler);
            }
            return sb.toString();
        }
    }

    @Deprecated
    public static String getCommonalities(final String a, final String b) {
        return getCommonPrefix(a, b);
    }

    /**
     * returns the part that equals in a & b abcdddddd & abcccccc = abc
     *
     * @param sameSource
     * @param sourceUrl
     * @return
     */
    public static String getCommonPrefix(final String a, final String b) {
        if (a == null) {
            return b;
        } else if (b == null) {
            return a;
        } else if (a == b) {
            return a;
        } else {
            int i = 0;
            final int aL = a.length();
            final int bL = b.length();
            final int max = Math.min(aL, bL);
            for (i = 0; i < max; i++) {
                if (a.charAt(i) != b.charAt(i)) {
                    break;
                }
            }
            if (i == 0) {
                return "";
            } else if (i == bL) {
                return b;
            } else if (i == aL) {
                return a;
            } else {
                return a.substring(0, i);
            }
        }
    }

    public static String getCommonPostfix(final String a, final String b) {
        if (a == null) {
            return b;
        } else if (b == null) {
            return a;
        } else if (a == b) {
            return a;
        } else {
            int i = 0;
            final int aL = a.length();
            final int bL = b.length();
            int max = Math.min(aL, bL);
            for (i = 0; i < max; i++) {
                if (a.charAt(aL - 1 - i) != b.charAt(bL - 1 - i)) {
                    break;
                }
            }
            if (i == 0) {
                return "";
            } else if (i == bL) {
                return b;
            } else if (i == aL) {
                return a;
            } else {
                return a.substring(aL - i);
            }
        }
    }

    public static String getCommonPrefix(final String... values) {
        String ret = null;
        final int len = values.length;
        for (int index = 0; index < len; index++) {
            final String value = values[index];
            if (ret == null) {
                ret = value;
            } else {
                ret = getCommonPrefix(ret, value);
                if (ret == null) {
                    return null;
                } else if (ret.length() == 0) {
                    return "";
                }
            }
        }
        return ret;
    }

    public static String getCommonPostfix(final String... values) {
        String ret = null;
        final int len = values.length;
        for (int index = 0; index < len; index++) {
            final String value = values[index];
            if (ret == null) {
                ret = value;
            } else {
                ret = getCommonPostfix(ret, value);
                if (ret == null) {
                    return null;
                } else if (ret.length() == 0) {
                    return "";
                }
            }
        }
        return ret;
    }

    // Keep for compatibility in webinstaller!
    public final static boolean isEmpty(final String value) {
        return value == null || value.length() == 0 || isEmptyAfterTrim(value);
    }

    public final static boolean isEmptyAfterTrim(final String value) {
        final int len = value.length();
        if (len == 0) {
            return true;
        } else {
            int st = 0;
            while (st < len && value.charAt(st) <= ' ') {
                st++;
            }
            return st == len;
        }
    }

    // Keep for compatibility in webinstaller!
    public final static boolean isNotEmpty(final String value) {
        return !isEmpty(value);
    }

    /**
     * Returns wether a String is null,empty, or contains whitespace only
     *
     * @param ip
     * @return
     */
    public final static boolean isAllEmpty(final String... values) {
        if (values != null) {
            final int len = values.length;
            for (int index = 0; index < len; index++) {
                final String value = values[index];
                if (value != null && !isEmptyAfterTrim(value)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * @param value
     * @return
     */
    public final static boolean isAllNotEmpty(final String... values) {
        if (values != null) {
            final int len = values.length;
            for (int index = 0; index < len; index++) {
                final String value = values[index];
                if (StringUtils.isEmpty(value)) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param parameters
     * @param string
     * @return
     */
    public static String join(Object[] parameters, String separator) {
        StringBuilder sb = new StringBuilder();
        for (Object s : parameters) {
            if (sb.length() > 0) {
                sb.append(separator);
            }
            sb.append(String.valueOf(s));
        }
        return sb.toString();
    }

    public static String join(String separator, Object... parameters) {
        StringBuilder sb = new StringBuilder();
        for (Object s : parameters) {
            if (sb.length() > 0) {
                sb.append(separator);
            }
            sb.append(String.valueOf(s));
        }
        return sb.toString();
    }

    /**
     * @param signParams
     * @param separator
     * @return
     */
    public static String join(Collection<? extends Object> params, String separator) {
        return join(params.toArray(new Object[] {}), separator);
    }

    public static interface Stringifier<T> {
        public String toString(T obj);
    }

    /**
     * @param <T>
     * @param requiredBy
     * @param string
     * @param stringifyer
     */
    public static <T> String join(Collection<T> params, String separator, Stringifier<T> stringifier) {
        StringBuilder sb = new StringBuilder();
        for (T o : params) {
            String s = stringifier.toString(o);
            if (s == null) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(separator);
            }
            sb.append(s);
        }
        return sb.toString();
    }

    /**
     * @param fcgiPorts
     * @param separator
     * @return
     */
    public static String join(String separator, int... ints) {
        StringBuilder sb = new StringBuilder();
        for (Object s : ints) {
            if (sb.length() > 0) {
                sb.append(separator);
            }
            sb.append(String.valueOf(s));
        }
        return sb.toString();
    }

    /**
     * @param string
     * @param length
     * @return returns a list of the given length and each entry contains string
     */
    public static List<String> createList(String string, int length) {
        final ArrayList<String> ret = new ArrayList<String>(length);
        while (length-- > 0) {
            ret.add(string);
        }
        return ret;
    }

    /**
     * a wrapper around String.replaceAll with nullcheck on the actual text
     *
     * @param string
     * @param string2
     * @param changeLogText
     * @return
     */
    public static String replaceAllByRegex(String regex, String replacement, String text) {
        if (text == null) {
            return text;
        } else {
            return text.replaceAll(regex, replacement);
        }
    }

    /**
     * @param defaultMessage
     * @param i
     * @param string
     * @return
     */
    public static String abr(String defaultMessage, int max, String postfox) {
        if (defaultMessage == null) {
            return null;
        } else if (defaultMessage.length() <= max) {
            return defaultMessage;
        } else {
            return defaultMessage.substring(0, max) + postfox;
        }
    }

    /**
     * @param name
     * @return
     */
    public static String toCamelCase(String name, boolean firstUpper) {
        if (name.length() == 0) {
            return name;
        } else {
            final StringBuilder sb = new StringBuilder(name.length());
            boolean firstFlag = true;
            for (String p : name.split("[\\_\\-\\s]+")) {
                if (p.length() > 0) {
                    final String first = p.substring(0, 1);
                    if (firstFlag) {
                        if (firstUpper) {
                            sb.append(first.toUpperCase(Locale.ENGLISH));
                        } else {
                            sb.append(first.toLowerCase(Locale.ENGLISH));
                        }
                        firstFlag = false;
                    } else {
                        sb.append(first.toUpperCase(Locale.ENGLISH));
                    }
                    sb.append(p, 1, p.length());
                }
            }
            return sb.toString();
        }
    }

    public static String shareAtLeastOne(String[] xArray, String[] yArray, final boolean caseInsensitive) {
        if (xArray == null || yArray == null || xArray.length == 0 || yArray.length == 0) {
            return null;
        }
        final HashSet<String> set = new HashSet<String>();
        for (final String x : xArray) {
            if (caseInsensitive) {
                set.add(x.toLowerCase(Locale.ENGLISH));
            } else {
                set.add(x);
            }
        }
        for (final String y : yArray) {
            final boolean contains;
            if (caseInsensitive) {
                contains = set.contains(y.toLowerCase(Locale.ENGLISH));
            } else {
                contains = set.contains(y);
            }
            if (contains) {
                return y;
            }
        }
        return null;
    }

    /**
     * @param value
     * @param the
     *            split result without empty entries. Returns an empty array for null input
     * @return
     */
    public static String[] splitNoEmpty(String value, String delim) {
        if (value == null) {
            return new String[] {};
        } else {
            final String splits[] = value.split(delim);
            final ArrayList<String> ret = new ArrayList<String>(splits.length);
            for (final String split : splits) {
                if (StringUtils.isNotEmpty(split)) {
                    ret.add(split);
                }
            }
            return ret.toArray(new String[0]);
        }
    }

    /**
     * @param string
     * @param string2
     * @return
     */
    public static String firstNotEmpty(String... entries) {
        for (String s : entries) {
            if (StringUtils.isNotEmpty(s)) {
                return s;
            }
        }
        return null;
    }

    public static int getPadLength(final int num) {
        if (num >= 0) {
            return String.valueOf(num).length();
        } else {
            throw new IllegalArgumentException("num must be >=0!:" + num);
        }
    }

    /**
     * @param object
     * @return
     */
    public static String valueOfOrNull(Object object) {
        if (object == null) {
            return null;
        } else {
            return String.valueOf(object);
        }
    }

    public static String toUpperCaseOrNull(String value, Locale locale) {
        if (value == null) {
            return null;
        } else {
            return value.toUpperCase(locale);
        }
    }

    public static String toUpperCaseOrNull(String value) {
        return toUpperCaseOrNull(value, Locale.ENGLISH);
    }

    public static String toLowerCaseOrNull(String value, Locale locale) {
        if (value == null) {
            return null;
        } else {
            return value.toLowerCase(locale);
        }
    }

    public static String toLowerCaseOrNull(String value) {
        return toLowerCaseOrNull(value, Locale.ENGLISH);
    }

    /**
     * @param key
     * @param b
     * @return
     */
    public static String camelCaseToUnderscore(final String key, final boolean uppercase) {
        final StringBuilder staticKey = new StringBuilder(key.length());
        char c, lastc;
        lastc = ' ';
        for (int i = 0; i < key.length(); i++) {
            c = key.charAt(i);
            if (staticKey.length() > 0) {
                if (Character.isUpperCase(c) && Character.isLowerCase(lastc)) {
                    staticKey.append('_');
                }
            }
            staticKey.append(uppercase ? Character.toUpperCase(c) : Character.toLowerCase(c));
            lastc = c;
        }
        return staticKey.toString();
    }

    public static ArrayList<CharSequence> camelCaseSplit(final CharSequence key) {
        final ArrayList<CharSequence> ret = new ArrayList<CharSequence>();
        char c, lastc;
        lastc = ' ';
        int offset = 0;
        for (int i = 0; i < key.length(); i++) {
            c = key.charAt(i);
            if (Character.isUpperCase(c) && Character.isLowerCase(lastc)) {
                ret.add(key.subSequence(offset, i));
                offset = i;
            }
            lastc = c;
        }
        if (offset < key.length()) {
            ret.add(key.subSequence(offset, key.length()));
        }
        return ret;
    }

    /**
     * @param trim
     * @param maxCharactersByLine
     * @return
     */
    public static String wrapText(String trim, int size) {
        return wrapText(trim, size, null, null, true);
    }

    /**
     * @param trim
     * @param size
     * @param compile
     * @return
     */
    public static final Pattern  NEWLINE             = Pattern.compile("[\r\n]{1,2}");
    public static final Pattern  DEFAULT_WRAP_BEFORE = Pattern.compile("(\\s)");
    private static final Pattern MATCHES_NEVER       = Pattern.compile("$^");
    public static final Pattern  DEFAULT_WRAP_AFTER  = MATCHES_NEVER;

    private final static class ReferencedCharArrayCharSequence implements CharSequence {
        private final char[] value;
        private final int    offset;
        private final int    length;

        private ReferencedCharArrayCharSequence(final CharSequence charSequence) {
            final int length = charSequence.length();
            value = new char[length];
            charSequence.toString().getChars(0, length, value, 0);
            this.offset = 0;
            this.length = length;
        }

        private ReferencedCharArrayCharSequence(final int offset, final int start, final int end, char[] value) {
            this.value = value;
            this.offset = offset + start;
            this.length = end - start;
        }

        @Override
        public final int length() {
            return this.length;
        }

        protected final void appendTo(StringBuilder sb, int length) {
            sb.append(value, offset, length);
        }

        @Override
        public final String toString() {
            return new String(value, offset, length);
        }

        @Override
        public final char charAt(int index) {
            return value[index + this.offset];
        }

        @Override
        public final ReferencedCharArrayCharSequence subSequence(int start, int end) {
            return new ReferencedCharArrayCharSequence(this.offset, start, end, value);
        }
    }

    public static String wrapText(String stringToWrap, int maxCharsPerLine, String patternWrapBefore, String patternWrapAfter, boolean forceWrapLongWords) {
        if (maxCharsPerLine == 0) {
            throw new IllegalArgumentException("Size must be > 0");
        } else {
            final Pattern patBefore = patternWrapBefore == null ? DEFAULT_WRAP_BEFORE : Pattern.compile(patternWrapBefore);
            final Pattern paAfter = patternWrapAfter == null ? DEFAULT_WRAP_AFTER : Pattern.compile(patternWrapAfter);
            return wrap(stringToWrap, maxCharsPerLine, forceWrapLongWords, patBefore, paAfter);
        }
    }

    public static String wrap(String stringToWrap, int maxCharsPerLine, boolean forceWrapLongWords, Pattern patternBefore, Pattern patternAfter) {
        if (maxCharsPerLine == 0) {
            throw new IllegalArgumentException("Size must be > 0");
        } else {
            final ReferencedCharArrayCharSequence value = new ReferencedCharArrayCharSequence(stringToWrap);
            return wrapInternal(value, new StringBuilder(), maxCharsPerLine, forceWrapLongWords, NEWLINE, patternBefore, patternAfter).toString();
        }
    }

    private static StringBuilder wrapInternal(ReferencedCharArrayCharSequence input, StringBuilder output, int maxCharsPerLine, boolean forceWrapLongWords, Pattern patternNewline, Pattern patternBefore, Pattern patternAfter) {
        Matcher newLineMatcherInstance = patternNewline == null ? null : patternNewline.matcher("");
        Matcher matcherBeforeInstance = patternBefore == null ? null : patternBefore.matcher("");
        Matcher matcherAfterInstance = patternAfter == null ? null : patternAfter.matcher("");
        boolean appendNewLine = output.length() > 0;
        while (input.length() > 0) {
            if (newLineMatcherInstance != null) {
                if (newLineMatcherInstance.reset(input).find()) {
                    final int start = newLineMatcherInstance.start();
                    if (start <= maxCharsPerLine) {
                        if (appendNewLine) {
                            output.append("\r\n");
                            input.appendTo(output, start);
                        } else {
                            input.appendTo(output, start);
                            appendNewLine = output.length() > 0;
                        }
                    } else {
                        final ReferencedCharArrayCharSequence subSequence = input.subSequence(0, start);
                        wrapInternal(subSequence, output, maxCharsPerLine, forceWrapLongWords, null, patternBefore, patternAfter);
                        if (!appendNewLine) {
                            appendNewLine = output.length() > 0;
                        }
                    }
                    input = input.subSequence(newLineMatcherInstance.end(), input.length());
                    continue;
                } else {
                    newLineMatcherInstance = null;
                }
            }
            if (input.length() < maxCharsPerLine) {
                if (appendNewLine) {
                    output.append("\r\n");
                }
                input.appendTo(output, input.length);
                return output;
            }
            int splitBeforeAt = 0;
            int splitAfterAt = 0;
            int last = 0;
            int replaceBeforeLength = -1;
            if (matcherBeforeInstance != null) {
                matcherBeforeInstance.reset(input);
                while (true) {
                    if (!matcherBeforeInstance.find()) {
                        matcherBeforeInstance = null;
                        break;
                    }
                    final int start = matcherBeforeInstance.start();
                    if (start > maxCharsPerLine) {
                        if (last == 0 && !forceWrapLongWords) {
                            replaceBeforeLength = -1;
                            splitBeforeAt = start;
                        } else {
                            splitBeforeAt = last;
                        }
                        break;
                    } else {
                        replaceBeforeLength = -1;
                        int gc = matcherBeforeInstance.groupCount();
                        for (int i = 1; i < gc + 1; i++) {
                            // search matcher groups, because we skip these groups. example: "split this" at space. if the space is
                            // within a
                            // matcher group, the result will be "split\r\nthis",else "split\r\n this"
                            final int groupStart = matcherBeforeInstance.start(i);
                            final int groupEnd = matcherBeforeInstance.end(i);
                            if (groupStart != -1 && groupEnd != -1) {
                                replaceBeforeLength = groupEnd - groupStart;
                                break;
                            }
                            // System.out.println(i + " - _" + matcherBefore.group(i) + "_");
                        }
                        last = start;
                        final int end = matcherBeforeInstance.end();
                        if (end == input.length()) {
                            splitBeforeAt = start;
                            break;
                        }
                    }
                }
            }
            splitBeforeAt = last;
            last = 0;
            if (matcherAfterInstance != null && patternAfter != DEFAULT_WRAP_AFTER) {
                matcherAfterInstance.reset(input);
                while (true) {
                    if (!matcherAfterInstance.find()) {
                        matcherAfterInstance = null;
                        break;
                    }
                    int end = matcherAfterInstance.end();
                    if (end > maxCharsPerLine || end == input.length()) {
                        if (last == 0 && !forceWrapLongWords) {
                            splitAfterAt = end;
                        } else {
                            splitAfterAt = last;
                        }
                        break;
                    } else {
                        last = end;
                    }
                }
            }
            splitAfterAt = last;
            int splitAT = Math.max(splitAfterAt, splitBeforeAt);
            if (splitAfterAt > maxCharsPerLine && splitBeforeAt <= maxCharsPerLine) {
                splitAT = splitBeforeAt;
            }
            if (splitBeforeAt > maxCharsPerLine && splitAfterAt <= maxCharsPerLine) {
                splitAT = splitAfterAt;
            }
            if (splitAT == 0) {
                if (forceWrapLongWords) {
                    splitAT = maxCharsPerLine;
                } else {
                    splitAT = input.length();
                }
            }
            splitAT = Math.min(splitAT, input.length());
            if (appendNewLine) {
                output.append("\r\n");
                input.appendTo(output, splitAT);
            } else {
                input.appendTo(output, splitAT);
                appendNewLine = output.length() > 0;
            }
            if (replaceBeforeLength > 0 && splitAT == splitBeforeAt) {
                splitAT += replaceBeforeLength;
            }
            input = input.subSequence(splitAT, input.length());
        }
        return output;
    }

    /**
     * Splits a text by newline and appends the intends in front of each line. If there are more lines than intends, the last one is used
     * for all extra lines.
     *
     * @param String
     * @param intend1
     *            ,2,3...
     * @return
     */
    public static String multiLineIntend(String text, String... intends) {
        StringBuilder sb = new StringBuilder();
        Matcher matcher = Pattern.compile("(\r\n)|(\r)|(\n)").matcher(text);
        int offset = 0;
        int lineCount = 0;
        while (matcher.find()) {
            sb.append(intends[Math.min(intends.length - 1, lineCount)]);
            sb.append(text, offset, matcher.start());
            sb.append(text, matcher.start(0), matcher.end(0));
            offset = matcher.end();
            lineCount++;
        }
        sb.append(intends[Math.min(intends.length - 1, lineCount)]);
        sb.append(text, offset, text.length());
        return sb.toString();
    }

    /**
     * @param replace
     * @return
     */
    public static String underscoreToCamelCase(String underscore) {
        final StringBuilder staticKey = new StringBuilder(underscore.length());
        char c;
        boolean nextUpper = false;
        for (int i = 0; i < underscore.length(); i++) {
            c = underscore.charAt(i);
            if (c == '_') {
                nextUpper = true;
                continue;
            }
            staticKey.append(nextUpper ? Character.toUpperCase(c) : Character.toLowerCase(c));
            nextUpper = false;
        }
        return staticKey.toString();
    }

    /**
     * @param latLnagQuery
     * @param addressQuery
     * @return
     */
    public static List<String> listWithoutEmptyStrings(String... strings) {
        ArrayList<String> ret = new ArrayList<String>(strings.length);
        for (String s : strings) {
            if (StringUtils.isNotEmpty(s)) {
                ret.add(s);
            }
        }
        return ret;
    }

    /**
     * @param string
     * @param string2
     * @return
     */
    public static String upperLowerAfterRegex(String pattern, String string, Boolean firstUpper, Boolean restUpper) {
        final String[] split = string.split("((?<=" + pattern + "))");
        final StringBuilder sb = new StringBuilder(string.length());
        for (final String s : split) {
            if (s.length() == 0) {
                continue;
            } else {
                if (firstUpper == Boolean.TRUE) {
                    sb.append(s.substring(0, 1).toUpperCase(Locale.ROOT));
                } else if (firstUpper == Boolean.FALSE) {
                    sb.append(s.substring(0, 1).toLowerCase(Locale.ROOT));
                } else {
                    sb.append(s, 0, 1);
                }
                if (s.length() > 1) {
                    if (restUpper == Boolean.TRUE) {
                        final String upperCase = s.toUpperCase(Locale.ROOT);
                        sb.append(upperCase, 1, upperCase.length());
                    } else if (restUpper == Boolean.FALSE) {
                        final String lowerCase = s.toLowerCase(Locale.ROOT);
                        sb.append(lowerCase, 1, lowerCase.length());
                    } else {
                        sb.append(s, 1, s.length());
                    }
                }
            }
        }
        return sb.toString();
    }

    /**
     * Regex version of string.indexOf
     *
     * @param alt
     * @param splitParts
     * @return
     */
    public static int indexOf(String input, String regex) {
        return indexOf(input, Pattern.compile(regex));
    }

    public static int indexOf(String input, int fromIndex, Pattern regex) {
        if (fromIndex >= input.length()) {
            return -1;
        } else {
            final Matcher matcher = regex.matcher(input);
            if (fromIndex <= 0) {
                fromIndex = 0;
            }
            if (!matcher.find(fromIndex)) {
                return -1;
            } else {
                final int ret = matcher.start();
                return ret;
            }
        }
    }

    /**
     * Regex version of string.indexOf
     *
     * @param input
     * @param compile
     * @return
     */
    public static int indexOf(String input, Pattern regex) {
        return indexOf(input, 0, regex);
    }

    /**
     * Calculates the string distance between source and target strings using the Damerau-Levenshtein algorithm. The distance is
     * case-sensitive. License: Public Domain (https://github.com/crwohlfeil/damerau-levenshtein)
     *
     * @param source
     *            The source String.
     * @param target
     *            The target String.
     * @return The distance between source and target strings.
     * @throws IllegalArgumentException
     *             If either source or target is null.
     */
    public static int damerauLevenshtein(CharSequence source, CharSequence target) {
        if (source == null || target == null) {
            throw new IllegalArgumentException("Parameter must not be null");
        }
        int sourceLength = source.length();
        int targetLength = target.length();
        if (sourceLength == 0) {
            return targetLength;
        }
        if (targetLength == 0) {
            return sourceLength;
        }
        int[][] dist = new int[sourceLength + 1][targetLength + 1];
        for (int i = 0; i < sourceLength + 1; i++) {
            dist[i][0] = i;
        }
        for (int j = 0; j < targetLength + 1; j++) {
            dist[0][j] = j;
        }
        for (int i = 1; i < sourceLength + 1; i++) {
            for (int j = 1; j < targetLength + 1; j++) {
                int cost = source.charAt(i - 1) == target.charAt(j - 1) ? 0 : 1;
                dist[i][j] = Math.min(Math.min(dist[i - 1][j] + 1, dist[i][j - 1] + 1), dist[i - 1][j - 1] + cost);
                if (i > 1 && j > 1 && source.charAt(i - 1) == target.charAt(j - 2) && source.charAt(i - 2) == target.charAt(j - 1)) {
                    dist[i][j] = Math.min(dist[i][j], dist[i - 2][j - 2] + cost);
                }
            }
        }
        return dist[sourceLength][targetLength];
    }

    public static int levenshtein(String strA, String strB, boolean caseINsensitive) {
        if (caseINsensitive) {
            strA = strA.toLowerCase(Locale.ROOT);
            strB = strB.toLowerCase(Locale.ROOT);
        }
        // init grid
        int[] costs = new int[strB.length() + 1];
        for (int i = 0; i < costs.length; i++) {
            costs[i] = i;
        }
        for (int i = 1; i <= strA.length(); i++) {
            int weigther = i - 1;
            costs[0] = i;
            for (int ii = 1; ii <= strB.length(); ii++) {
                int cj = Math.min(1 + Math.min(costs[ii], costs[ii - 1]), strA.charAt(i - 1) == strB.charAt(ii - 1) ? weigther : weigther + 1);
                weigther = costs[ii];
                costs[ii] = cj;
            }
        }
        return costs[strB.length()];
    }

    /**
     * Joins all strings and put the seperator between - however this method igores empty or null Stings
     *
     * @param string
     * @param lit_login_error
     * @param localizedMessage
     */
    public static String joinNonEmpty(String separator, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String s : parts) {
            if (StringUtils.isEmpty(s)) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(separator);
            }
            sb.append(s);
        }
        return sb.toString();
    }

    /**
     * @param library
     * @param version2
     * @param description2
     * @param filesHashes
     * @param licenses2
     * @return
     */
    public static int getMaxLength(String... strings) {
        int ret = 0;
        for (String s : strings) {
            ret = Math.max(s.length(), ret);
        }
        return ret;
    }
}
