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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author thomas
 *
 */
public class Regex {
    @Deprecated
    public static String escape(final String pattern) {
        return Pattern.quote(pattern);
    }

    public static String[] getLines(final String arg) {
        if (arg == null) {
            return new String[] {};
        } else {
            final String[] temp = arg.split("(\r\n|\r|\n)");
            final int tempLength = temp.length;
            final String[] output = new String[tempLength];
            for (int i = 0; i < tempLength; i++) {
                output[i] = temp[i].trim();
            }
            return output;
        }
    }

    @Deprecated
    public static boolean matches(final Object str, final Pattern pat) {
        return new Regex(str, pat).matches();
    }

    @Deprecated
    public static boolean matches(final Object page, final String string) {
        return new Regex(page, string).matches();
    }

    /**
     * @param sslResponse
     * @param string
     * @param string2
     * @return
     */
    public static String replace(final String text, final String regex, final String replacement) {
        return Pattern.compile(regex, Pattern.DOTALL | Pattern.MULTILINE).matcher(text).replaceAll(replacement);
    }

    private Matcher matcher;

    public Regex(final Matcher matcher) {
        this.matcher = matcher;
    }

    public Regex(final Object data, final Pattern pattern) {
        this(data instanceof CharSequence ? (CharSequence) data : StringUtils.valueOfOrNull(data), pattern);
    }

    public Regex(final Object data, final String pattern) {
        this(data instanceof CharSequence ? (CharSequence) data : StringUtils.valueOfOrNull(data), pattern);
    }

    public Regex(final Object data, final String pattern, final int flags) {
        this(data instanceof CharSequence ? (CharSequence) data : StringUtils.valueOfOrNull(data), pattern, flags);
    }

    public Regex(final String data, final Pattern pattern) {
        this((CharSequence) data, pattern);
    }

    public Regex(final CharSequence data, final Pattern pattern) {
        if (data != null && pattern != null) {
            this.matcher = pattern.matcher(data);
        }
    }

    public Regex(final String data, final String pattern) {
        this((CharSequence) data, pattern);
    }

    public Regex(final CharSequence data, final String pattern) {
        if (data != null && pattern != null) {
            this.matcher = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE | Pattern.DOTALL).matcher(data);
        }
    }

    public Regex(final CharSequence data, final String pattern, final int flags) {
        if (data != null && pattern != null) {
            this.matcher = Pattern.compile(pattern, flags).matcher(data);
        }
    }

    public Regex(final String data, final String pattern, final int flags) {
        this((CharSequence) data, pattern, flags);
    }

    public int count() {
        final Matcher matcher = this.matcher;
        if (matcher == null) {
            return 0;
        } else {
            matcher.reset();
            int c = 0;
            while (matcher.find()) {
                c++;
            }
            return c;
        }
    }

    public String[] getColumn(int x) {
        final Matcher matcher = this.matcher;
        if (matcher == null) {
            return null;
        } else {
            x++;
            matcher.reset();
            final java.util.List<String> ar = new ArrayList<String>();
            while (matcher.find()) {
                final String tmp = matcher.group(x);
                ar.add(tmp);
            }
            return ar.toArray(new String[ar.size()]);
        }
    }

    public Pattern getPattern() {
        final Matcher matcher = this.matcher;
        return matcher != null ? matcher.pattern() : null;
    }

    public String getMatch(final int group) {
        final Matcher matcher = this.matcher;
        if (matcher != null) {
            matcher.reset();
            if (matcher.find()) {
                final String ret = matcher.group(group + 1);
                return ret;
            }
        }
        return null;
    }

    public String getMatch(int entry, final int group) {
        final Matcher matcher = this.matcher;
        if (matcher != null) {
            matcher.reset();
            // group++;
            entry++;
            int groupCount = 0;
            while (matcher.find()) {
                if (groupCount == group) {
                    final String ret = matcher.group(entry);
                    return ret;
                }
                groupCount++;
            }
        }
        return null;
    }

    public Matcher getMatcher() {
        final Matcher matcher = this.matcher;
        if (matcher != null) {
            matcher.reset();
        }
        return matcher;
    }

    /**
     * Gibt alle Treffer eines Matches in einem 2D array aus
     *
     * @return
     */
    public String[][] getMatches() {
        final Matcher matcher = this.matcher;
        if (matcher == null) {
            return null;
        } else {
            matcher.reset();
            final java.util.List<String[]> ar = new ArrayList<String[]>();
            while (matcher.find()) {
                final int c = matcher.groupCount();
                int d = 1;
                String[] group;
                if (c == 0) {
                    group = new String[c + 1];
                    d = 0;
                } else {
                    group = new String[c];
                }
                for (int i = d; i <= c; i++) {
                    final String tmp = matcher.group(i);
                    group[i - d] = tmp;
                }
                ar.add(group);
            }
            return ar.size() == 0 ? new String[][] {} : ar.toArray(new String[][] {});
        }
    }

    public String[] getRow(final int y) {
        final Matcher matcher = this.matcher;
        if (matcher != null) {
            matcher.reset();
            int groupCount = 0;
            while (matcher.find()) {
                if (groupCount == y) {
                    final int c = matcher.groupCount();
                    final String[] group = new String[c];
                    for (int i = 1; i <= c; i++) {
                        final String tmp = matcher.group(i);
                        group[i - 1] = tmp;
                    }
                    return group;
                }
                groupCount++;
            }
        }
        return null;
    }

    public boolean patternFind() {
        final Matcher matcher = this.matcher;
        if (matcher == null) {
            return false;
        } else {
            matcher.reset();
            return matcher.find();
        }
    }

    @Deprecated
    /**
     * @Deprecated because does not matches but find!
     * @return
     */
    public boolean matches() {
        return patternFind();
    }

    public boolean patternMatches() {
        final Matcher matcher = this.matcher;
        if (matcher == null) {
            return false;
        } else {
            matcher.reset();
            return matcher.matches();
        }
    }

    @Deprecated
    public void setMatcher(final Matcher matcher) {
        this.matcher = matcher;
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder();
        final String[][] matches = this.getMatches();
        final int matchesLength = matches.length;
        String[] match;
        int matchLength;
        for (int i = 0; i < matchesLength; i++) {
            match = matches[i];
            matchLength = match.length;
            for (int j = 0; j < matchLength; j++) {
                ret.append("match[");
                ret.append(i);
                ret.append("][");
                ret.append(j);
                ret.append("] = ");
                ret.append(match[j]);
                ret.append(System.getProperty("line.separator"));
            }
        }
        final Matcher matcher = this.matcher;
        if (matcher != null) {
            matcher.reset();
        }
        return ret.toString();
    }
}