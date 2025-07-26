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
package jd.parser.html;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jd.nutils.encoding.Encoding;

import org.appwork.utils.Application;
import org.appwork.utils.CharSequenceUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Hex;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.PublicSuffixList;
import org.appwork.utils.net.URLHelper;

public class HTMLParser {
    private static class ConcatCharSequence implements CharSequence {
        private final CharSequence[] charSequences;
        private final int            offset;
        private final int            end;
        private final int            length;

        public ConcatCharSequence(final CharSequence... charSequences) {
            this.charSequences = charSequences;
            int length = 0;
            for (final CharSequence charSequence : charSequences) {
                length += charSequence.length();
            }
            this.offset = 0;
            this.end = length;
            this.length = length;
        }

        private ConcatCharSequence(final int offset, final int start, final int end, final ConcatCharSequence concatCharSequence) {
            this.charSequences = concatCharSequence.charSequences;
            this.end = end;
            this.offset = offset + start;
            this.length = end - start;
        }

        @Override
        public int length() {
            return this.length;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder(this.length());
            sb.ensureCapacity(this.length());
            for (int index = 0; index < this.length(); index++) {
                sb.append(this.charAt(index));
            }
            return sb.toString();
        }

        @Override
        public char charAt(int index) {
            index = index + this.offset;
            final int offsetEnd = this.end + this.offset;
            if (index < 0 || index >= offsetEnd) {
                throw new IndexOutOfBoundsException("Index " + index);
            }
            int range = 0;
            for (final CharSequence charSequence : this.charSequences) {
                if (index < offsetEnd && index < range + charSequence.length()) {
                    return charSequence.charAt(index - range);
                }
                range += charSequence.length();
            }
            throw new IndexOutOfBoundsException("Index " + index);
        }

        @Override
        public CharSequence subSequence(final int start, final int end) {
            return new ConcatCharSequence(this.offset, start, end, this);
        }
    }

    public static class HtmlParserCharSequenceHierarchy {
        private final HtmlParserCharSequence          htmlParserCharSequence;
        private final HtmlParserCharSequenceHierarchy previous;

        public HtmlParserCharSequenceHierarchy(final HtmlParserCharSequence htmlParserCharSequence) {
            this(null, htmlParserCharSequence);
        }

        public HtmlParserCharSequenceHierarchy(final HtmlParserCharSequenceHierarchy previous, final HtmlParserCharSequence htmlParserCharSequence) {
            this.previous = previous;
            this.htmlParserCharSequence = htmlParserCharSequence;
        }

        public HtmlParserCharSequenceHierarchy previousPath() {
            return this.previous;
        }

        public HtmlParserCharSequence lastElement() {
            return this.htmlParserCharSequence;
        }

        public boolean contains(final HtmlParserCharSequence htmlParserCharSequence) {
            HtmlParserCharSequenceHierarchy current = this;
            while (current != null) {
                final HtmlParserCharSequence elem = current.lastElement();
                if (elem != null && elem.equals(htmlParserCharSequence)) {
                    return true;
                } else {
                    current = current.previousPath();
                }
            }
            return false;
        }
    }

    /**
     * this is an optimized CharSequence to reduce memory/performance impact of new string generation during substring/regex/replace stuff
     *
     * @author daniel
     *
     */
    public static class HtmlParserCharSequence implements CharSequence {
        final char[]                   chars;
        final CharSequence             charSequence;
        final CharSequence             source;
        final int                      start;
        final int                      end;
        HashMap<Pattern, Boolean>      findsPattern         = null;
        HashMap<Pattern, Boolean>      matchesPattern       = null;
        HashMap<CharSequence, Boolean> containsCharSequence = null;

        private HtmlParserCharSequence(final HtmlParserCharSequence source, final int start, final int end) {
            this.chars = source.chars;
            this.charSequence = source.charSequence;
            this.source = source;
            this.start = start;
            this.end = end;
        }

        /**
         * Pattern doesn't implement hashCode or equals
         *
         * @param map
         * @param key
         * @return
         */
        private Boolean getValue(final Map<Pattern, Boolean> map, final Pattern key) {
            if (map == null || map.size() == 0) {
                return null;
            } else {
                final Boolean ret = map.get(key);
                if (ret != null || map.containsKey(key)) {
                    return ret;
                } else {
                    for (final Entry<Pattern, Boolean> entry : map.entrySet()) {
                        if (entry.getKey().pattern().equals(key.pattern()) && entry.getKey().flags() == key.flags()) {
                            return entry.getValue();
                        }
                    }
                }
                return null;
            }
        }

        private HtmlParserCharSequence(final CharSequence source) {
            this.chars = null;
            this.charSequence = source;
            this.source = source;
            this.start = 0;
            this.end = source.length();
        }

        public int getRetainedLength() {
            if (this.chars != null) {
                return this.chars.length;
            } else {
                return this.charSequence.length();
            }
        }

        @Override
        public char charAt(int index) {
            index = index + this.getStartIndex();
            if (index > this.getStopIndex()) {
                throw new IndexOutOfBoundsException("index " + index + " > end " + this.getStopIndex());
            }
            return this.rawCharAt(index);
        }

        protected char rawCharAt(final int index) {
            if (this.chars != null) {
                return this.chars[index];
            } else {
                return this.charSequence.charAt(index);
            }
        }

        int hashCodeCache = 0;
        int hashCode      = -1;

        @Override
        public int hashCode() {
            if (this.hashCodeCache != this.hashCode) {
                int h = 0;
                final int length = this.length();
                if (length > 0) {
                    for (int i = 0; i < length; i++) {
                        h = 31 * h + this.charAt(i);
                    }
                }
                this.hashCodeCache = h;
                this.hashCode = h;
                return h;
            }
            return this.hashCodeCache;
        }

        protected <T> T getSource(final Class<? extends CharSequence> T) {
            final CharSequence ret = this.source;
            if (T.isAssignableFrom(ret.getClass())) {
                return (T) ret;
            } else {
                return null;
            }
        }

        public boolean contains(final CharSequence s) {
            Boolean result = this.containsCharSequence != null ? this.containsCharSequence.get(s) : null;
            if (result != null) {
                return result.booleanValue();
            }
            final HtmlParserCharSequence source = this.getSource(HtmlParserCharSequence.class);
            if (source != null && source.containsCharSequence != null) {
                result = source.containsCharSequence.get(s);
                // if source doesn't contain CharSequence, then this won't contain it neither
                // but doesn't work the other way round as this can just be a subSequence
                if (Boolean.FALSE.equals(result)) {
                    return false;
                }
            }
            result = this.indexOf(s, 0) >= 0;
            if (this.containsCharSequence == null) {
                this.containsCharSequence = new HashMap<CharSequence, Boolean>();
            }
            this.containsCharSequence.put(s, result);
            return result.booleanValue();
        }

        public boolean equals(final CharSequence anotherString) {
            if (this == anotherString) {
                return true;
            }
            if (anotherString != null) {
                int n = this.length();
                if (n == anotherString.length()) {
                    int i = 0;
                    while (n-- != 0) {
                        if (this.charAt(i) != anotherString.charAt(i)) {
                            return false;
                        }
                        i++;
                    }
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean equals(final Object anObject) {
            if (this == anObject) {
                return true;
            } else if (anObject != null && anObject instanceof CharSequence) {
                return this.equals((CharSequence) anObject);
            } else {
                return false;
            }
        }

        public int getStartIndex() {
            return this.start;
        }

        public int getStopIndex() {
            return this.end;
        }

        protected int indexOf(final int sourceOffset, final int sourceCount, final CharSequence target, final int targetOffset, final int targetCount, int fromIndex) {
            if (fromIndex >= sourceCount) {
                return targetCount == 0 ? sourceCount : -1;
            }
            if (fromIndex < 0) {
                fromIndex = 0;
            }
            if (targetCount == 0) {
                return fromIndex;
            }
            final char first = target.charAt(targetOffset);
            final int max = sourceOffset + sourceCount - targetCount;
            if (this.chars != null) {
                /* we have a char array */
                for (int i = sourceOffset + fromIndex; i <= max; i++) {
                    /* Look for first character. */
                    if (this.chars[i] != first) {
                        while (++i <= max && this.chars[i] != first) {
                            ;
                        }
                    }
                    /* Found first character, now look at the rest of v2 */
                    if (i <= max) {
                        int j = i + 1;
                        final int end = j + targetCount - 1;
                        for (int k = targetOffset + 1; j < end && this.chars[j] == target.charAt(k); j++, k++) {
                            ;
                        }
                        if (j == end) {
                            /* Found whole string. */
                            return i - sourceOffset;
                        }
                    }
                }
            } else {
                /* we have a charSequence */
                for (int i = sourceOffset + fromIndex; i <= max; i++) {
                    /* Look for first character. */
                    if (this.rawCharAt(i) != first) {
                        while (++i <= max && this.rawCharAt(i) != first) {
                            ;
                        }
                    }
                    /* Found first character, now look at the rest of v2 */
                    if (i <= max) {
                        int j = i + 1;
                        final int end = j + targetCount - 1;
                        for (int k = targetOffset + 1; j < end && this.rawCharAt(j) == target.charAt(k); j++, k++) {
                            ;
                        }
                        if (j == end) {
                            /* Found whole string. */
                            return i - sourceOffset;
                        }
                    }
                }
            }
            return -1;
        }

        public int indexOf(final CharSequence str) {
            return this.indexOf(str, 0);
        }

        public int count(final CharSequence str, final int countMax) {
            return this.count(str, 0, countMax);
        }

        public int count(final CharSequence str, final int fromIndex, final int countMax) {
            int ret = 0;
            int lastIndex = fromIndex;
            while (true) {
                final int index = this.indexOf(str, lastIndex);
                if (index != -1) {
                    ret++;
                    lastIndex = index;
                    if (countMax > 0 && ret >= countMax) {
                        break;
                    }
                } else {
                    break;
                }
            }
            return ret;
        }

        public int indexOf(final CharSequence indexOf, final int fromIndex) {
            return this.indexOf(this.getStartIndex(), this.length(), indexOf, 0, indexOf.length(), fromIndex);
        }

        @Override
        public int length() {
            return this.getStopIndex() - this.getStartIndex();
        }

        public boolean matches(final Pattern regex) {
            Boolean result = this.getValue(this.matchesPattern, regex);
            if (result != null) {
                return result.booleanValue();
            }
            final HtmlParserCharSequence source = this.getSource(HtmlParserCharSequence.class);
            if (source != null && source.matchesPattern != null) {
                result = this.getValue(source.matchesPattern, regex);
                // if source doesn't match Pattern, then this won't match it neither
                // but doesn't work the other way round as this can just be a subSequence
                // only trust pattern on source if it doesn't contain for start/end-marker
                if (!regex.pattern().contains("^") && !regex.pattern().contains("$") && Boolean.FALSE.equals(result)) {
                    return false;
                }
            }
            result = regex.matcher(this).matches();
            if (this.matchesPattern == null) {
                this.matchesPattern = new HashMap<Pattern, Boolean>();
            }
            this.matchesPattern.put(regex, result);
            return result.booleanValue();
        }

        public boolean find(final Pattern regex) {
            Boolean result = this.getValue(this.findsPattern, regex);
            if (result != null) {
                return result.booleanValue();
            }
            final HtmlParserCharSequence source = this.getSource(HtmlParserCharSequence.class);
            if (source != null && source.findsPattern != null) {
                result = this.getValue(source.findsPattern, regex);
                // if source doesn't find Pattern, then this won't find it neither
                // but doesn't work the other way round as this can just be a subSequence
                // only trust pattern on source if it doesn't contain for start/end-marker
                if (!regex.pattern().contains("^") && !regex.pattern().contains("$") && Boolean.FALSE.equals(result)) {
                    return false;
                }
            }
            result = regex.matcher(this).find();
            if (this.findsPattern == null) {
                this.findsPattern = new HashMap<Pattern, Boolean>();
            }
            this.findsPattern.put(regex, result);
            return result.booleanValue();
        }

        public int count(final Pattern regex, final int countMax) {
            final boolean findResult = this.find(regex);
            if (!findResult) {
                return 0;
            } else if (findResult && countMax == 1) {
                return 1;
            }
            final Matcher matcher = regex.matcher(this);
            int ret = 0;
            while (matcher.find()) {
                ret++;
                if (ret >= countMax) {
                    break;
                }
            }
            return ret;
        }

        public HtmlParserCharSequence replaceAll(final Pattern regex, final String replacement) {
            if (replacement == null) {
                throw new NullPointerException("replacement");
            } else if (!this.find(regex)) {
                return this;
            } else {
                final Matcher matcher = regex.matcher(this);
                matcher.reset();
                if (!matcher.find()) {
                    return this;
                } else {
                    final StringBuffer sb = new StringBuffer();
                    do {
                        matcher.appendReplacement(sb, replacement);
                    } while (matcher.find());
                    matcher.appendTail(sb);
                    if (this.equals(sb)) {
                        return this;
                    } else {
                        return new HtmlParserCharSequence(sb);
                    }
                }
            }
        }

        public HtmlParserCharSequence replaceFirst(final Pattern regex, final String replacement) {
            if (replacement == null) {
                throw new NullPointerException("replacement");
            } else if (!this.find(regex)) {
                return this;
            } else {
                final Matcher matcher = regex.matcher(this);
                matcher.reset();
                if (!matcher.find()) {
                    return this;
                } else {
                    final StringBuffer sb = new StringBuffer();
                    matcher.appendReplacement(sb, replacement);
                    matcher.appendTail(sb);
                    if (this.equals(sb)) {
                        return this;
                    } else {
                        return new HtmlParserCharSequence(sb);
                    }
                }
            }
        }

        public boolean endsWith(final String suffix) {
            return this.startsWith(suffix, this.length() - suffix.length());
        }

        public boolean startsWith(final CharSequence prefix) {
            return this.startsWith(prefix, 0);
        }

        public boolean startsWith(final CharSequence prefix, final int toffset) {
            int to = toffset;
            int po = 0;
            int pc = prefix.length();
            // Note: toffset might be near -1>>>1.
            if (toffset < 0 || toffset > this.length() - pc) {
                return false;
            }
            while (--pc >= 0) {
                if (this.charAt(to++) != prefix.charAt(po++)) {
                    return false;
                }
            }
            return true;
        }

        public List<HtmlParserCharSequence> getColumn(final int group, final Pattern pattern) {
            final List<HtmlParserCharSequence> result = new ArrayList<HtmlParserCharSequence>();
            if (!this.find(pattern)) {
                return result;
            } else {
                final Matcher matcher = pattern.matcher(this);
                while (true) {
                    final HtmlParserCharSequence match = this.group(group, matcher);
                    if (match != null) {
                        result.add(match);
                    } else {
                        break;
                    }
                }
                return result;
            }
        }

        private HtmlParserCharSequence group(final int group, final Matcher matcher) {
            if (!matcher.find()) {
                return null;
            } else {
                return this.subSequence(matcher.start(group), matcher.end(group));
            }
        }

        public HtmlParserCharSequence group(final int group, final Pattern pattern) {
            if (!this.find(pattern)) {
                return null;
            } else {
                final Matcher matcher = pattern.matcher(this);
                return this.group(group, matcher);
            }
        }

        public HtmlParserCharSequence subSequence(final int start) {
            if (start > 0) {
                return new HtmlParserCharSequence(this, this.getStartIndex() + start, this.getStopIndex());
            } else {
                return this;
            }
        }

        @Override
        public HtmlParserCharSequence subSequence(final int start, final int end) {
            if (start > 0 || end < this.getStopIndex()) {
                return new HtmlParserCharSequence(this, this.getStartIndex() + start, this.getStartIndex() + end);
            } else {
                return this;
            }
        }

        @Override
        public String toString() {
            if (this.chars != null) {
                return new String(this.chars, this.getStartIndex(), this.length());
            } else {
                return this.charSequence.subSequence(this.getStartIndex(), this.getStopIndex()).toString();
            }
        }

        public String toURL() {
            return new StringBuilder().append(this.toCharSequenceURL()).toString();
        }

        public CharSequence toCharSequenceURL() {
            int slash = 0;
            for (int index = 0; index < this.length(); index++) {
                if (this.charAt(index) == '/' && ++slash == 3) {
                    if (this.length() - index > 1) {
                        final CharSequence host = this.subSequence(0, index);
                        final CharSequence rest = this.subSequence(index);
                        final ConcatCharSequence ret = new ConcatCharSequence(host, Encoding.urlEncodeCharSequence_light(rest));
                        return ret;
                    } else {
                        return this;
                    }
                }
            }
            return this;
        }

        public HtmlParserCharSequence trim() {
            int len = this.length();
            int st = 0;
            while (st < len && this.charAt(st) <= ' ') {
                st++;
            }
            while (st < len && this.charAt(len - 1) <= ' ') {
                len--;
            }
            return st > 0 || len < this.length() ? this.subSequence(st, len) : this;
        }
    }

    public static class HtmlParserResultSet {
        protected final ArrayList<HtmlParserCharSequence> results     = new ArrayList<HtmlParserCharSequence>();
        protected final HashSet<HtmlParserCharSequence>   dupeCheck   = new HashSet<HTMLParser.HtmlParserCharSequence>();
        private HtmlParserCharSequence                    baseURL     = null;
        private Boolean                                   skipBaseURL = null;

        protected void setSkipBaseURL(final Boolean skipBaseURL) {
            this.skipBaseURL = skipBaseURL;
        }

        protected Boolean isSkipBaseURL() {
            return this.skipBaseURL;
        }

        protected HtmlParserResultSet stop() {
            return this;
        }

        protected HtmlParserResultSet start(final HtmlParserCharSequence data) {
            return this;
        }

        public HtmlParserCharSequence getBaseURL() {
            return this.baseURL;
        }

        private void setBaseURL(final HtmlParserCharSequence baseURL) {
            if (baseURL != null && !baseURL.equals("about:blank")) {
                this.baseURL = baseURL;
            }
        }

        public boolean add(final HtmlParserCharSequence e) {
            if (e != null) {
                if (this.dupeCheck.add(e)) {
                    this.results.add(e);
                    return true;
                }
            }
            return false;
        }

        public LogInterface getLogger() {
            return null;
        }

        public boolean remove(final HtmlParserCharSequence e) {
            if (e != null) {
                if (this.dupeCheck.remove(e)) {
                    this.results.remove(e);
                    return true;
                }
            }
            return false;
        }

        public int getLastResultIndex() {
            return this.results.size();
        }

        protected List<HtmlParserCharSequence> getResults() {
            return this.results;
        }

        protected Collection<String> exportResults() {
            final ArrayList<String> ret = new ArrayList<String>();
            for (final HtmlParserCharSequence result : this.getResults()) {
                final String url = result.toURL();
                if (StringUtils.isNotEmpty(url)) {
                    ret.add(url);
                }
            }
            return ret;
        }

        public List<HtmlParserCharSequence> getResultsSublist(final int index) {
            return this.results.subList(index, this.results.size());
        }

        public boolean contains(final HtmlParserCharSequence data) {
            return data != null && this.dupeCheck.contains(data);
        }
    }

    final static class Httppattern {
        public Pattern p;
        public int     group;

        public Httppattern(final Pattern p, final int group) {
            this.p = p;
            this.group = group;
        }
    }

    final private static Httppattern[] linkAndFormPattern     = new Httppattern[] { new Httppattern(Pattern.compile("src\\s*=\\s*('|\\\\\"|\")(.*?)(\\1)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL), 2), new Httppattern(Pattern.compile("(<\\s*a[^>]*href\\s*=\\s*|<\\s*form[^>]*action\\s*=\\s*)('|\\\\\"|\")(.*?)(\\2)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL), 3), new Httppattern(Pattern.compile("(<\\s*a[^>]*href\\s*=\\s*|<\\s*form[^>]*action\\s*=\\s*)([^'\"][^\\s]*)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL), 2), new Httppattern(Pattern.compile("\\[(link|url)\\](.*?)\\[/(link|url)\\]", Pattern.CASE_INSENSITIVE | Pattern.DOTALL), 2) };
    public final static String         protocolFile           = "file:/";
    final private static String        protocolPrefixes       = "((?:chrome|directhttp://https?|usenet|flashget|https?viajd|m3u8://https?|https?|ccf|dlc|ftp|ftpviajd|jd|rsdf|jdlist|ipfs|nxm|youtubev2" + (!Application.isJared(null) ? "|jdlog" : "") + ")(?:(:|\\\\x3A|\\\\u003A)(/|\\\\x2F|\\\\u002F)(/|\\\\x2F|\\\\u002F))|" + HTMLParser.protocolFile + "|magnet:|mega:)";
    final private static Pattern[]     basePattern            = new Pattern[] { Pattern.compile("<[^>]*base[^>]*href\\s*=\\s*('|\")(.*?)\\1", Pattern.CASE_INSENSITIVE), Pattern.compile("<[^>]*base[^>]*(href)\\s*=\\s*([^'\"][^\\s]*)", Pattern.CASE_INSENSITIVE) };
    final private static Pattern[]     hrefPattern            = new Pattern[] { Pattern.compile("data-\\w+\\s*=\\s*('|\")\\s*(.*?)\\s*(\\1)", Pattern.CASE_INSENSITIVE), Pattern.compile("href\\s*=\\s*('|\")\\s*(.*?)\\s*(\\1)", Pattern.CASE_INSENSITIVE), Pattern.compile("src\\s*=\\s*('|\")\\s*(.*?)\\s*(\\1)", Pattern.CASE_INSENSITIVE) };
    final private static Pattern       pat1                   = Pattern.compile("(" + HTMLParser.protocolPrefixes + "|(?<!://)www\\.)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern       protocols              = Pattern.compile("(" + HTMLParser.protocolPrefixes + ")");
    final private static Pattern       LINKPROTOCOL           = Pattern.compile("^" + HTMLParser.protocolPrefixes, Pattern.CASE_INSENSITIVE);
    final private static Pattern       mergePattern_Root      = Pattern.compile("(.*?(\\.[a-z0-9]+|\\])(:\\d+)?)(/|$)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern       mergePattern_Directory = Pattern.compile("(.*?(\\.[a-z0-9]+|\\])(:\\d+)?/[^?#]+/)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern       mergePattern_Path      = Pattern.compile("(.*?(\\.[a-z0-9]+|\\])(:\\d+)?/[^?#]+)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static Pattern             mp                     = null;
    static {
        try {
            HTMLParser.mp = Pattern.compile("(\\\\\"|\"|')?((" + HTMLParser.protocolPrefixes + "|www\\.).+?(?=((\\s+" + HTMLParser.protocolPrefixes + ")|\\1|<|>|\\[/|\r|\n|\f|\t|$|';|'\\)|\"\\s*|'\\+)))", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        } catch (final Throwable e) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
        }
    }

    private static final class HtmlParserOptions {
        private final boolean reverse;

        private boolean isReverse() {
            return this.reverse;
        }

        private boolean isHex() {
            return this.hex;
        }

        private boolean isBase64() {
            return this.base64;
        }

        private boolean isUrlEncoded() {
            return this.urlEncoded;
        }

        private boolean isUnicode() {
            return this.unicode;
        }

        private boolean isHtmlDecode() {
            return this.htmlDecode;
        }

        private final boolean hex;
        private final boolean base64;
        private final boolean urlEncoded;
        private final boolean unescape;
        private final boolean unicode;
        private final boolean htmlDecode;

        private boolean isUnescape() {
            return this.unescape;
        }

        private HtmlParserOptions() {
            this(true, true, true, true, true, true, true);
        }

        private HtmlParserOptions(final boolean reverse, final boolean hex, final boolean base64, final boolean urlEncoded, final boolean unescape, final boolean unicode, final boolean htmlDecode) {
            this.reverse = reverse;
            this.hex = hex;
            this.base64 = base64;
            this.urlEncoded = urlEncoded;
            this.unescape = unescape;
            this.unicode = unicode;
            this.htmlDecode = htmlDecode;
        }

        private HtmlParserOptions reverse(final boolean reverse) {
            return new HtmlParserOptions(reverse, this.hex, this.base64, this.urlEncoded, this.unescape, this.unicode, this.htmlDecode);
        }

        private HtmlParserOptions hex(final boolean hex) {
            return new HtmlParserOptions(this.reverse, hex, this.base64, this.urlEncoded, this.unescape, this.unicode, this.htmlDecode);
        }

        private HtmlParserOptions base64(final boolean base64) {
            return new HtmlParserOptions(this.reverse, this.hex, base64, this.urlEncoded, this.unescape, this.unicode, this.htmlDecode);
        }

        private HtmlParserOptions urlEncoded(final boolean urlEncoded) {
            return new HtmlParserOptions(this.reverse, this.hex, this.base64, urlEncoded, this.unescape, this.unicode, this.htmlDecode);
        }

        private HtmlParserOptions unescape(final boolean unescape) {
            return new HtmlParserOptions(this.reverse, this.hex, this.base64, this.urlEncoded, unescape, this.unicode, this.htmlDecode);
        }

        private HtmlParserOptions unicode(final boolean unicode) {
            return new HtmlParserOptions(this.reverse, this.hex, this.base64, this.urlEncoded, this.unescape, unicode, this.htmlDecode);
        }

        private HtmlParserOptions htmlDecode(final boolean decode) {
            return new HtmlParserOptions(this.reverse, this.hex, this.base64, this.urlEncoded, this.unescape, this.unicode, decode);
        }
    }

    private final static int                    MIN_VALID                   = "ftp://1.23".length();
    final private static Pattern                checkPatternBase64          = Pattern.compile("([A-Za-z0-9+/]{16,}={0,2})", Pattern.DOTALL);
    final private static Pattern                checkPatternUnicode         = Pattern.compile("(\\\\u[0-9a-fA-F]{2,})", Pattern.DOTALL);
    final private static Pattern                unescapePattern             = Pattern.compile("unescape\\(('|\")(.*?)(\\1)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern                checkPatternHREFUNESCAPESRC = Pattern.compile("(href\\s*=|unescape|src\\s*=).", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern                checkPatternHREFSRC         = Pattern.compile(".*?(href\\s*=|src\\s*=).+", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern                unhexPattern                = Pattern.compile("(([0-9a-fA-F]{2}){" + HTMLParser.MIN_VALID + ",})");
    final private static Pattern                paramsCut1                  = Pattern.compile("://[^\r\n\"' ]*?/[^\r\n\"']+\\?.[^\r\n\"']*?=([^\r\n\"']*?)($|\r|\n|\"|')", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    final private static Pattern                paramsCut2                  = Pattern.compile("://[^\r\n\"' ]*?/[^\r\n\"']*?\\?([^\r\n\"']*?)($|\r|\n|\"|')", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern                inTagsPattern               = Pattern.compile("<([^<]*)>");
    final private static Pattern                endTagPattern               = Pattern.compile("^([^>]*)>", Pattern.DOTALL);
    final private static Pattern                taglessPattern              = Pattern.compile("^(.*)$", Pattern.DOTALL);
    final private static HtmlParserCharSequence directHTTP                  = new HtmlParserCharSequence("directhttp://");
    final private static HtmlParserCharSequence httpviajd                   = new HtmlParserCharSequence("httpviajd://");
    final private static HtmlParserCharSequence httpsviajd                  = new HtmlParserCharSequence("httpsviajd://");
    final private static HtmlParserCharSequence http                        = new HtmlParserCharSequence("http://");
    final private static Pattern                dummyCnl                    = Pattern.compile("https?://dummycnl.jdownloader.org/#?[a-f0-9A-F]+");
    final private static Pattern                httpRescue                  = Pattern.compile("h.{2,3}://", Pattern.CASE_INSENSITIVE);
    final private static Pattern                tagsPattern                 = Pattern.compile(".*<.*>.*", Pattern.DOTALL);
    final private static Pattern                singleSpacePattern          = Pattern.compile(" ");
    final private static Pattern                findSpacePattern            = Pattern.compile("\\s");
    final private static Pattern                hdotsPattern                = Pattern.compile("h.{2,3}://", Pattern.CASE_INSENSITIVE);
    final private static Pattern                specialReplacePattern       = Pattern.compile("'");
    final private static Pattern                questionMarkPattern         = Pattern.compile("\\?");
    final private static Pattern                maybeHostPattern            = Pattern.compile("^([^/]+)/.+");
    final private static Pattern                missingHTTPPattern          = Pattern.compile("^www\\.", Pattern.CASE_INSENSITIVE);
    // full | double full | partial | partial | partial | partial | partial | partial
    final private static Pattern                urlEncodedProtocol          = Pattern.compile("(%3A%2F%2F|%253A%252F%252F|%3A//|%3A%2F/|%3A/%2F|:%2F%2F|:%2F/|:/%2F)", Pattern.CASE_INSENSITIVE);

    private static HtmlParserCharSequenceHierarchy next(final HtmlParserCharSequenceHierarchy path, final HtmlParserCharSequence next) {
        if (path == null) {
            return new HtmlParserCharSequenceHierarchy(next);
        } else if (next != null && !next.equals(path.lastElement())) {
            return new HtmlParserCharSequenceHierarchy(path, next);
        } else {
            return path;
        }
    }

    private static HtmlParserResultSet _getHttpLinksDeepWalker(HtmlParserCharSequenceHierarchy path, HtmlParserResultSet results, HtmlParserOptions options) {
        if (options == null) {
            options = new HtmlParserOptions();
        }
        if (results == null) {
            results = new HtmlParserResultSet();
        }
        HtmlParserCharSequence data = path.lastElement();
        results = results.start(data);
        if (data != null) {
            data = data.trim();
            path = HTMLParser.next(path, data);
        }
        if (data == null || data.length() < HTMLParser.MIN_VALID) {
            return results.stop();
        }
        if ((data.startsWith(HTMLParser.directHTTP) || data.startsWith(HTMLParser.httpviajd) || data.startsWith(HTMLParser.httpsviajd)) && results.contains(data)) {
            /* we don't have to further check urls with those prefixes */
            return results.stop();
        } else if (data.matches(HTMLParser.dummyCnl)) {
            return results.stop();
        }
        final int indexBefore = results.getLastResultIndex();
        /* find reversed */
        if (options.isReverse()) {
            final CharSequence reversedata = new StringBuilder(data).reverse();
            if (!data.equals(reversedata)) {
                final HtmlParserCharSequence next = new HtmlParserCharSequence(reversedata);
                final boolean historyContains = path.contains(next);
                if (!historyContains) {
                    HTMLParser._getHttpLinksFinder(HTMLParser.next(path, next), results, options.reverse(false));
                    HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, next), results, options);
                }
            }
        }
        /* find base64'ed */
        final HtmlParserCharSequence urlDecoded = HTMLParser.decodeURLParamEncodedURL(path, false);
        // check for non base64 chars here -> speed up
        if (options.isBase64()) {
            final List<HtmlParserCharSequence> base64Matches = data.getColumn(1, HTMLParser.checkPatternBase64);
            for (HtmlParserCharSequence base64Match : base64Matches) {
                final int padding = 4 - base64Match.length() % 4;
                if (padding != 0) {
                    switch (padding) {
                    case 1:
                        base64Match = new HtmlParserCharSequence(new ConcatCharSequence(base64Match, "="));
                        break;
                    case 2:
                        base64Match = new HtmlParserCharSequence(new ConcatCharSequence(base64Match, "=="));
                        break;
                    default:
                        break;
                    }
                }
                final CharSequence base64Data = Encoding.Base64Decode(base64Match);
                if (base64Data.length() > 0 && (!urlDecoded.equals(base64Data) || !data.equals(base64Data))) {
                    final HtmlParserCharSequenceHierarchy nextPath = HTMLParser.next(path, base64Match);
                    final HtmlParserCharSequence next = new HtmlParserCharSequence(base64Data);
                    final boolean historyContains = path.contains(next);
                    if (!historyContains) {
                        HTMLParser._getHttpLinksFinder(HTMLParser.next(nextPath, next), results, options.base64(false));
                        final int undefined = next.count("�", 1);
                        if (undefined == 0) {
                            HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(nextPath, next), results, options);
                        }
                    }
                }
            }
        }
        if (options.isUnescape()) {
            /* parse escaped js stuff */
            if (data.length() > 23 && data.contains("unescape")) {
                final List<HtmlParserCharSequence> unescaped = data.getColumn(2, HTMLParser.unescapePattern);
                if (unescaped != null && unescaped.size() > 0) {
                    for (final HtmlParserCharSequence unescape : unescaped) {
                        final HtmlParserCharSequenceHierarchy nextPath = HTMLParser.next(path, unescape);
                        final HtmlParserCharSequence next = new HtmlParserCharSequence(new HtmlParserCharSequence(Encoding.htmlDecode(unescape.toString())));
                        final boolean historyContains = path.contains(next);
                        if (!historyContains) {
                            HTMLParser._getHttpLinksFinder(HTMLParser.next(nextPath, next), results, options.unescape(false));
                            final int undefined = next.count("�", 1);
                            if (undefined == 0) {
                                HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(nextPath, next), results, options);
                            }
                        }
                    }
                }
            }
        }
        if (options.isHex()) {
            /* find hex'ed */
            if (HTMLParser.deepWalkCheck(path, options, results, indexBefore) && data.length() >= 24) {
                HtmlParserCharSequence hex = data.replaceAll(HTMLParser.singleSpacePattern, "");
                hex = data.group(1, HTMLParser.unhexPattern);
                if (hex != null && hex.length() >= 24) {
                    try {
                        /* remove spaces from hex-coded string */
                        hex = hex.replaceAll(HTMLParser.singleSpacePattern, "");
                        final HtmlParserCharSequenceHierarchy nextPath = HTMLParser.next(path, hex);
                        final String hexString = Hex.hex2String(hex);
                        final HtmlParserCharSequence next = new HtmlParserCharSequence(hexString);
                        final boolean historyContains = path.contains(next);
                        if (!historyContains) {
                            HTMLParser._getHttpLinksFinder(HTMLParser.next(nextPath, next), results, options.hex(false));
                            final int undefined = next.count("�", 1);
                            if (undefined == 0) {
                                HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(nextPath, next), results, options);
                            }
                        }
                    } catch (final Throwable e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                    }
                }
            }
        }
        if (HTMLParser.checkForUnicode(options, data)) {
            /* no changes in results size, and data contains unicode */
            final HtmlParserCharSequence next = HTMLParser.unicodeDecode(urlDecoded);
            final boolean historyContains = path.contains(next);
            if (!historyContains) {
                if (urlDecoded.find(HTMLParser.checkPatternUnicode)) {
                    HTMLParser._getHttpLinksFinder(HTMLParser.next(path, next), results, options);
                    HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, next), results, options);
                } else {
                    HTMLParser._getHttpLinksFinder(HTMLParser.next(path, next), results, options.unicode(false));
                    HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, next), results, options);
                }
            }
        }
        if (options.isUrlEncoded()) {
            if (HTMLParser.deepWalkCheck(path, options, results, indexBefore) && !urlDecoded.equals(data)) {
                /* no changes in results size, and data contains urlcoded http://, so lets urldecode it */
                final HtmlParserCharSequence next = new HtmlParserCharSequence(urlDecoded);
                final boolean historyContains = path.contains(next);
                if (!historyContains) {
                    if (urlDecoded.find(HTMLParser.urlEncodedProtocol)) {
                        HTMLParser._getHttpLinksFinder(HTMLParser.next(path, next), results, options);
                        HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, next), results, options);
                    } else {
                        HTMLParser._getHttpLinksFinder(HTMLParser.next(path, next), results, options.urlEncoded(false));
                        HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, next), results, options);
                    }
                }
            }
        }
        return results.stop();
    }

    private static void addToResultSet(final HtmlParserCharSequenceHierarchy path, final HtmlParserResultSet results, final HtmlParserOptions options) {
        if (results.add(HTMLParser.correctURL(path, results, options))) {
            final HtmlParserCharSequence data = path.lastElement();
            if (data.find(HTMLParser.urlEncodedProtocol)) {
                int lastIndexOf = 0;
                while (true) {
                    int indexOf = data.indexOf("?", lastIndexOf);
                    if (indexOf == -1) {
                        indexOf = data.indexOf("&", lastIndexOf);
                    }
                    if (indexOf != -1) {
                        final HtmlParserCharSequence subSequence = data.subSequence(lastIndexOf, indexOf);
                        lastIndexOf = indexOf + 1;
                        if (subSequence.find(HTMLParser.urlEncodedProtocol)) {
                            final HtmlParserCharSequenceHierarchy next = HTMLParser.next(path, subSequence);
                            final HtmlParserCharSequence urlDecoded = HTMLParser.decodeURLParamEncodedURL(next, true);
                            HTMLParser._getHttpLinksWalker(HTMLParser.next(next, urlDecoded), results, null, options);
                        }
                        // TODO: check for other embedded urls, base64, reverse...
                    } else {
                        break;
                    }
                }
            }
        }
    }

    private static boolean skip(final HtmlParserCharSequence link) {
        return link == null || link.equals("about:blank") || link.equals("/") || link.startsWith("#");
    }

    protected static class HexHtmlParserCharSequence extends HtmlParserCharSequence {
        protected HexHtmlParserCharSequence(final CharSequence charSequence) {
            super(charSequence);
        }

        @Override
        protected char rawCharAt(final int index) {
            final char h1 = this.charSequence.charAt(index * 4 + 2);
            final char h2 = this.charSequence.charAt(index * 4 + 3);
            return (char) Long.parseLong("" + h1 + h2, 16);
        }

        @Override
        public int getStopIndex() {
            return super.getStopIndex() / 4;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            for (int i = this.getStartIndex(); i < this.getStopIndex(); i++) {
                sb.append(this.charAt(i));
            }
            return sb.toString();
        }
    }

    protected static HtmlParserCharSequence convert(final HtmlParserCharSequence sequence) {
        if (sequence.length() > 2 && sequence.length() % 4 == 0 && sequence.charAt(0) == '\\' && sequence.charAt(1) == 'x') {
            return new HexHtmlParserCharSequence(sequence);
        } else {
            return sequence;
        }
    }

    private final static Pattern escapedJSONPattern = Pattern.compile("\\\\");

    private static HtmlParserResultSet _getHttpLinksFinder(HtmlParserCharSequenceHierarchy path, HtmlParserResultSet results, final HtmlParserOptions options) {
        if (results == null) {
            results = new HtmlParserResultSet();
        }
        HtmlParserCharSequence data = path.lastElement();
        results = results.start(data);
        if (data != null) {
            data = data.trim();
            path = HTMLParser.next(path, data);
        }
        if (data == null || data.length() == 0) {
            return results.stop();
        }
        if (data.contains(":\\/\\/")) {
            /**
             * \\ escaped urls, eg JSON
             */
            data = data.replaceAll(HTMLParser.escapedJSONPattern, "");
        }
        if (!data.matches(HTMLParser.tagsPattern)) {
            final int c = data.count(HTMLParser.pat1, 2);
            if (c == 0) {
                if (!data.matches(HTMLParser.checkPatternHREFSRC)) {
                    /* maybe url without protocol */
                    if (data.count(HTMLParser.questionMarkPattern, 2) < 2) {
                        final HtmlParserCharSequence location = HTMLParser.parseLocation(results, results.getBaseURL(), data, true);
                        if (HTMLParser.getProtocol(location) != null) {
                            return HTMLParser._getHttpLinksFinder(HTMLParser.next(path, location), results, options);
                        }
                        final HtmlParserCharSequence host = data.group(1, HTMLParser.maybeHostPattern);
                        if (host != null && host.equals(PublicSuffixList.getInstance().getDomain(host.toString()))) {
                            final HtmlParserCharSequence missingProtocol = new HtmlParserCharSequence(new ConcatCharSequence("http://", data));
                            return HTMLParser._getHttpLinksFinder(HTMLParser.next(path, missingProtocol), results, options);
                        }
                    /* no href inside */}
                    return results.stop();
                }
            } else if (c == 1 && data.length() < 256) {
                HtmlParserCharSequence protocol = null;
                HtmlParserCharSequence link = data;
                if ((protocol = HTMLParser.getProtocol(link)) == null && !link.contains("%2F")) {
                    link = data.replaceFirst(HTMLParser.hdotsPattern, "http://").replaceFirst(HTMLParser.missingHTTPPattern, "http://www.");
                }
                if ((protocol = HTMLParser.getProtocol(link)) != null) {
                    if (protocol.startsWith(HTMLParser.protocolFile)) {
                        HTMLParser.addToResultSet(HTMLParser.next(path, link), results, options);
                        return results.stop();
                    } else {
                        while (link.length() > 2 && link.charAt(0) == '<' && link.charAt(link.length() - 1) == '>') {
                            link = link.subSequence(1, link.length() - 1);
                        }
                        while (link.length() > 2 && link.charAt(0) == '\"' && link.charAt(link.length() - 1) == '\"') {
                            link = link.subSequence(1, link.length() - 1);
                        }
                        if (!link.find(HTMLParser.findSpacePattern)) {
                            HTMLParser.addToResultSet(HTMLParser.next(path, link), results, options);
                            return results.stop();
                        }
                    }
                }
            }
        }
        final HtmlParserCharSequence baseURL = HTMLParser.getBaseURL(data, results);
        for (final Pattern pattern : HTMLParser.hrefPattern) {
            final List<HtmlParserCharSequence> hrefs = data.getColumn(2, pattern);
            for (HtmlParserCharSequence hrefURL : hrefs) {
                hrefURL = HTMLParser.convert(hrefURL);
                if (!HTMLParser.skip(hrefURL)) {
                    if (baseURL != null) {
                        hrefURL = HTMLParser.parseLocation(results, baseURL, hrefURL);
                    } else {
                        /* no baseURL available, lets give a hint with http protocol */
                        hrefURL = HTMLParser.parseLocation(results, HTMLParser.http, hrefURL);
                    }
                    if (HTMLParser.getProtocol(hrefURL) != null) {
                        /* found a valid url with recognized protocol */
                        HTMLParser.addToResultSet(HTMLParser.next(path, hrefURL), results, options);
                    }
                }
            }
        }
        for (final Httppattern element : HTMLParser.linkAndFormPattern) {
            final List<HtmlParserCharSequence> links = data.getColumn(element.group, element.p);
            for (HtmlParserCharSequence link : links) {
                link = HTMLParser.convert(link);
                if (!HTMLParser.skip(link)) {
                    HtmlParserCharSequence protocol = HTMLParser.getProtocol(link);
                    if (protocol == null) {
                        link = link.replaceAll(HTMLParser.httpRescue, "http://");
                        if (baseURL != null) {
                            link = HTMLParser.parseLocation(results, baseURL, link);
                        } else {
                            /* no baseURL available, lets give a hint with http protocol */
                            link = HTMLParser.parseLocation(results, HTMLParser.http, link);
                        }
                        protocol = HTMLParser.getProtocol(link);
                    }
                    if (protocol != null) {
                        HTMLParser.addToResultSet(HTMLParser.next(path, link), results, options);
                    }
                }
            }
        }
        if (HTMLParser.mp != null) {
            final Matcher m = HTMLParser.mp.matcher(data);
            HtmlParserCharSequence link = null;
            while ((link = data.group(2, m)) != null) {
                link = HTMLParser.convert(link);
                link = link.trim();
                final HtmlParserCharSequence protocol = HTMLParser.getProtocol(link);
                if (protocol == null && !link.contains("%2F")) {
                    link = link.replaceFirst(HTMLParser.missingHTTPPattern, "http://www\\.");
                }
                HTMLParser.addToResultSet(HTMLParser.next(path, link), results, options);
                if (protocol != null && protocol.startsWith("directhttp://")) {
                    /*
                     * special handling for directhttp, do not search for inner links!
                     */
                    continue;
                }
                final Matcher mlinks = HTMLParser.protocols.matcher(link);
                int start = -1;
                /*
                 * special handling if we have multiple links without newline separation
                 */
                while (mlinks.find()) {
                    if (start == -1) {
                        start = mlinks.start();
                    } else {
                        final HtmlParserCharSequence link2 = link.subSequence(start, mlinks.start());
                        HTMLParser.addToResultSet(HTMLParser.next(path, link2), results, options);
                        start = mlinks.start();
                    }
                }
                if (start > 0) {
                    link = link.subSequence(start);
                    HTMLParser.addToResultSet(HTMLParser.next(path, link), results, options);
                }
                if (data.equals(link)) {
                    /* data equals check, so we can leave this loop */
                    return results.stop();
                }
            }
        }
        return results.stop();
    }

    private static HtmlParserCharSequence getBaseURL(final HtmlParserCharSequence data, final HtmlParserResultSet results) {
        HtmlParserCharSequence baseURL = results.getBaseURL();
        if (results.isSkipBaseURL() == null) {
            HtmlParserCharSequence htmlBaseURL = null;
            for (final Pattern pattern : HTMLParser.basePattern) {
                final HtmlParserCharSequence found = data.group(2, pattern);
                if (!HTMLParser.skip(found)) {
                    htmlBaseURL = found;
                    break;
                }
            }
            if (htmlBaseURL != null && HTMLParser.getProtocol(htmlBaseURL) != null) {
                results.setBaseURL(htmlBaseURL);
                baseURL = htmlBaseURL;
                results.setSkipBaseURL(true);
            } else {
                results.setSkipBaseURL(false);
            }
        }
        return baseURL;
    }

    private final static String TAGOPEN  = String.valueOf('<');
    private final static String TAGCLOSE = String.valueOf('>');

    private static HtmlParserResultSet _getHttpLinksWalker(HtmlParserCharSequenceHierarchy path, HtmlParserResultSet results, Pattern tagRegex, HtmlParserOptions options) {
        // System.out.println("Call: "+data.length()); )
        if (options == null) {
            options = new HtmlParserOptions();
        }
        if (results == null) {
            results = new HtmlParserResultSet();
        }
        HtmlParserCharSequence data = path.lastElement();
        results = results.start(data);
        if (data != null) {
            data = data.trim();
            path = HTMLParser.next(path, data);
        }
        if (data == null || data.length() < HTMLParser.MIN_VALID) {
            return results.stop();
        }
        if (results.isSkipBaseURL() == null) {
            /* it is important to search baseURL in FULL data */
            HTMLParser.getBaseURL(data, results);
        }
        /* filtering tags, recursion command me ;) */
        while (true) {
            if (tagRegex == null) {
                tagRegex = HTMLParser.inTagsPattern;
            }
            final HtmlParserCharSequence nexttag = data.group(1, tagRegex);
            if (nexttag == null || nexttag.length() == 0) {
                /* no further tag found, lets continue */
                break;
            } else {
                /* lets check if tag contains links */
                HTMLParser._getHttpLinksWalker(HTMLParser.next(path, nexttag), results, HTMLParser.inTagsPattern, options);
                final int tagOpen = data.indexOf(new ConcatCharSequence(HTMLParser.TAGOPEN, nexttag));
                int tagClose = -1;
                if (tagOpen >= 0) {
                    tagClose = tagOpen + nexttag.length() + 1;
                }
                if (tagClose >= 0 && data.length() >= tagClose + 1) {
                    if (tagOpen > 0) {
                        /*
                         * there might be some data left before the tag, do not remove that data
                         */
                        final HtmlParserCharSequence dataLeft = data.subSequence(0, tagOpen);
                        final HtmlParserCharSequence dataLeft2 = data.subSequence(tagClose + 1);
                        data = null;
                        if (dataLeft.contains(HTMLParser.TAGCLOSE)) {
                            HTMLParser._getHttpLinksWalker(HTMLParser.next(path, dataLeft), results, HTMLParser.endTagPattern, options);
                        } else {
                            HTMLParser._getHttpLinksWalker(HTMLParser.next(path, dataLeft), results, HTMLParser.taglessPattern, options);
                        }
                        data = dataLeft2;
                    } else {
                        /* remove tag at begin of data */
                        data = data.subSequence(tagClose + 1);
                        if (data.length() == 0) {
                            return results.stop();
                        }
                    }
                    // System.out.println("SubCall: "+data.length());
                } else {
                    if (tagClose < 0) {
                        data = data.subSequence(nexttag.length());
                        if (data.length() == 0) {
                            return results.stop();
                        }
                    } else {
                        /* remove tag at begin of data */
                        data = data.subSequence(tagClose + 1);
                        if (data.length() == 0) {
                            return results.stop();
                        }
                    }
                }
            }
        }
        /* find normal */
        if (data.length() < HTMLParser.MIN_VALID) {
            //
            return results.stop();
        }
        if (!data.contains("://") && !data.contains(HTMLParser.protocolFile) && !data.contains(":\\/\\/") && !data.contains("www.") && !HTMLParser.protocols.matcher(data).find()) {
            if (HTMLParser.checkForBase64(options, data)) {
            } else if (HTMLParser.checkForReverse(options, data)) {
            } else if (HTMLParser.checkForUrlEncoded(options, data)) {
            } else if (HTMLParser.checkForUnescape(options, data)) {
            } else if (HTMLParser.checkForUnicode(options, data)) {
                /* maybe easy encrypted website or a href */
            } else {
                return results.stop();
            }
        }
        final int indexBefore = results.getResults().size();
        HTMLParser._getHttpLinksFinder(path, results, options);
        if (HTMLParser.deepWalkCheck(path, options, results, indexBefore)) {
            HTMLParser._getHttpLinksDeepWalker(path, results, options);
            /* cut of ?xy= parts if needed */
            HtmlParserCharSequence newdata = data.group(1, HTMLParser.paramsCut1);
            if (newdata != null && !data.equals(newdata)) {
                HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, newdata), results, options);
            }
            /* use of ?xy parts if available */
            newdata = data.group(1, HTMLParser.paramsCut2);
            if (newdata != null && !data.equals(newdata)) {
                HTMLParser._getHttpLinksDeepWalker(HTMLParser.next(path, newdata), results, options);
            }
        }
        return results.stop();
    }

    private static boolean checkForReverse(final HtmlParserOptions options, final HtmlParserCharSequence data) {
        return (options == null || options.isReverse()) && (data.contains("//:ptth") || data.contains("//:sptth") || data.contains("//:ptf"));
    }

    private static boolean checkForUrlEncoded(final HtmlParserOptions options, final HtmlParserCharSequence data) {
        return (options == null || options.isUrlEncoded()) && data.find(HTMLParser.urlEncodedProtocol);
    }

    private static boolean checkForBase64(final HtmlParserOptions options, final HtmlParserCharSequence data) {
        return (options == null || options.isBase64()) && data.find(HTMLParser.checkPatternBase64);
    }

    private static boolean checkForUnicode(final HtmlParserOptions options, final HtmlParserCharSequence data) {
        return (options == null || options.isUnicode()) && data.find(HTMLParser.checkPatternUnicode);
    }

    private static boolean checkForUnescape(final HtmlParserOptions options, final HtmlParserCharSequence data) {
        return (options == null || options.isUnescape()) && data.find(HTMLParser.checkPatternHREFUNESCAPESRC);
    }

    private static void logInfo(final HtmlParserResultSet results, final String log) {
        if (results != null && log != null) {
            final LogInterface logger = results.getLogger();
            if (logger != null) {
                logger.info(log);
            }
        }
    }

    private static void logThrowable(final HtmlParserResultSet results, final Throwable throwable) {
        if (results != null && throwable != null) {
            final LogInterface logger = results.getLogger();
            if (logger != null) {
                logger.log(throwable);
            }
        }
    }

    private final static Pattern findParamPattern = Pattern.compile("^&[a-zA-Z0-9%]+=.{0,1}");

    private static HtmlParserCharSequence correctURL(final HtmlParserCharSequenceHierarchy path, final HtmlParserResultSet results, final HtmlParserOptions options) {
        HtmlParserCharSequence input = path.lastElement();
        if (input.contains("\\x3A)") || input.contains("\\x2F")) {
            input = HTMLParser.unicodeDecode(input);
        } else if (input.contains("\\u003A)") || input.contains("\\u002F")) {
            input = HTMLParser.unicodeDecode(input);
        }
        final int specialCutOff = input.indexOf("', ");
        if (specialCutOff >= 0) {
            final HtmlParserCharSequence cutoff = input.subSequence(0, specialCutOff);
            HTMLParser.logInfo(results, "Apply auto special cut off|" + input + "|" + cutoff);
            input = cutoff;
        }
        if (options.isHtmlDecode()) {
            input = HTMLParser.htmlDecode(input);
        }
        final int indexofa = input.indexOf("&");
        if (indexofa > 0 && input.indexOf("?") == -1) {
            final int indexofb = input.indexOf("#");
            if (indexofb < 0 || indexofb > indexofa) {
                /**
                 * this can happen when we found a link as urlparameter
                 *
                 * eg test.com/?u=http%3A%2F%2Fwww...&bla=
                 *
                 * then we get
                 *
                 * http://www...&bla
                 *
                 * we cut of the invalid urlParameter &bla
                 *
                 * check if we really have &x=y format following
                 *
                 * also pay attention about anchor
                 */
                final HtmlParserCharSequence check = input.subSequence(indexofa);
                if (check.find(HTMLParser.findParamPattern)) {
                    boolean autoCutOff = false;
                    // only autoCutOff when we find the explained special case in it
                    HtmlParserCharSequenceHierarchy currentPath = path;
                    while (currentPath != null) {
                        final HtmlParserCharSequence element = currentPath.lastElement();
                        currentPath = currentPath.previousPath();
                        if (element == input || element == null) {
                            continue;
                        } else if (element.find(Pattern.compile("((&|\\?)[^=]*=?|[a-z_0-9%]=)" + Pattern.quote(input.toString())))) {
                            autoCutOff = true;
                            break;
                        }
                    }
                    if (autoCutOff) {
                        /* we have found &x=y pattern, so it is okay to cut it off */
                        final HtmlParserCharSequence cutoff = input.subSequence(0, indexofa);
                        HTMLParser.logInfo(results, "Apply auto cut off|" + input + "|" + cutoff);
                        input = cutoff;
                    }
                }
            }
        }
        try {
            final URL url = URLHelper.createURL(input.toString());
            final String originalPath = url.getPath();
            if (originalPath != null) {
                String pathString = originalPath;
                pathString = HTMLParser.specialReplacePattern.matcher(pathString).replaceAll("%27");
                pathString = HTMLParser.singleSpacePattern.matcher(pathString).replaceAll("%20");
                if (!originalPath.equals(pathString)) {
                    final String ret = URLHelper.createURL(url.getProtocol(), url.getUserInfo(), url.getHost(), url.getPort(), pathString, url.getQuery(), url.getRef());
                    return new HtmlParserCharSequence(ret);
                }
            }
            if (input.equals(url.toString())) {
                return input;
            } else {
                return new HtmlParserCharSequence(url.toString());
            }
        } catch (final Throwable e) {
            final HtmlParserCharSequence protocol = HTMLParser.getProtocol(input);
            if (protocol == null) {
                HTMLParser.logThrowable(results, e);
            }
        }
        return input;
    }

    private final static LinkedHashMap<Pattern, String> URLDECODE = new LinkedHashMap<Pattern, String>();
    static {
        // has to be first. to allow for multiple double encode of % eg. %253A%252F%252F
        HTMLParser.URLDECODE.put(Pattern.compile("%25"), "%");
        // rest can be in any order
        HTMLParser.URLDECODE.put(Pattern.compile("%2F"), "/");
        HTMLParser.URLDECODE.put(Pattern.compile("%3A"), ":");
        HTMLParser.URLDECODE.put(Pattern.compile("%3F"), "?");
        HTMLParser.URLDECODE.put(Pattern.compile("%3D"), "=");
        HTMLParser.URLDECODE.put(Pattern.compile("%26"), "&");
        HTMLParser.URLDECODE.put(Pattern.compile("%23"), "#");
    }

    private static HtmlParserCharSequence decodeURLParamEncodedURL(final HtmlParserCharSequenceHierarchy path, final boolean forceDecode) {
        HtmlParserCharSequence url = path.lastElement();
        if (url != null && (forceDecode || url.find(HTMLParser.urlEncodedProtocol))) {
            for (final Entry<Pattern, String> replace : HTMLParser.URLDECODE.entrySet()) {
                url = url.replaceAll(replace.getKey(), replace.getValue());
            }
        }
        return url;
    }

    private static boolean deepWalkCheck(final HtmlParserCharSequenceHierarchy path, final HtmlParserOptions options, final HtmlParserResultSet results, final int indexBefore) {
        final int latestIndex = results.getLastResultIndex();
        final boolean ret = latestIndex == indexBefore;
        if (!ret) {
            final List<HtmlParserCharSequence> subList = results.getResultsSublist(indexBefore);
            for (final HtmlParserCharSequence check : subList) {
                if (HTMLParser.checkForReverse(options, check)) {
                    return true;
                } else if (HTMLParser.checkForUnescape(options, check)) {
                    return true;
                } else if (HTMLParser.checkForUrlEncoded(options, check)) {
                    return true;
                } else if (HTMLParser.checkForBase64(options, check)) {
                    return true;
                } else if (HTMLParser.checkForUnicode(options, check)) {
                    return true;
                }
            }
        } else {
            final HtmlParserCharSequence data = path.lastElement();
            if (HTMLParser.checkForReverse(options, data)) {
                return true;
            } else if (HTMLParser.checkForUnescape(options, data)) {
                return true;
            } else if (HTMLParser.checkForUrlEncoded(options, data)) {
                return true;
            } else if (HTMLParser.checkForBase64(options, data)) {
                return true;
            } else if (HTMLParser.checkForUnicode(options, data)) {
                return true;
            }
        }
        return ret;
    }

    /**
     * Diese Methode sucht die vordefinierten input type="hidden" und formatiert sie zu einem poststring z.b. wÃ¼rde bei:
     *
     * <input type="hidden" name="f" value="f50b0f" /> <input type="hidden" name="h" value="390b4be0182b85b0" /> <input type="hidden"
     * name="b" value="9" />
     *
     * f=f50b0f&h=390b4be0182b85b0&b=9 ausgegeben werden
     *
     * @param data
     *            Der zu durchsuchende Text
     *
     * @return ein String, der als POST Parameter genutzt werden kann und alle Parameter des Formulars enthÃ¤lt
     */
    public static String getFormInputHidden(final String data) {
        return HTMLParser.joinMap(HTMLParser.getInputHiddenFields(data), "=", "&");
    }

    public static String[] getHttpLinks(final String data, final String url) {
        return HTMLParser.getHttpLinks(data, url, null);
    }

    public static String[] getHttpLinks(final String data, final String url, final HtmlParserResultSet results) {
        Collection<String> links = HTMLParser.getHttpLinksIntern(data, url, results);
        if (links == null || links.size() == 0) {
            return new String[0];
        }
        /*
         * in case we have valid and invalid (...) urls for the same link, we only use the valid one
         */
        final LinkedHashSet<String> tmplinks = new LinkedHashSet<String>(links.size());
        next: while (links.size() > 0) {
            final Iterator<String> it = links.iterator();
            while (it.hasNext()) {
                final String link = it.next();
                it.remove();
                if (link.contains("...")) {
                    String search = Pattern.quote(link);
                    search = search.replaceAll("(\\.{3,})([^\\.])", Matcher.quoteReplacement("\\E") + ".*?" + Matcher.quoteReplacement("\\Q") + "$2");
                    final Iterator<String> it2 = links.iterator();
                    while (it2.hasNext()) {
                        final String replacement = it2.next();
                        if (!replacement.contains("...") && replacement.matches("(?i)" + search)) {
                            tmplinks.add(replacement);
                            it2.remove();
                            continue next;
                        }
                    }
                }
                tmplinks.add(link);
            }
        }
        links = null;
        return tmplinks.toArray(new String[tmplinks.size()]);
    }

    public static Collection<String> getHttpLinksIntern(final String content, final String baseURLString) {
        return HTMLParser.getHttpLinksIntern(content, baseURLString, null);
    }

    private final static Pattern ltPattern   = Pattern.compile("&lt;");
    private final static Pattern gtPattern   = Pattern.compile("&gt;");
    private final static Pattern ampPattern  = Pattern.compile("&amp;");
    private final static Pattern quotPattern = Pattern.compile("&quot;");
    private final static Pattern brPattern   = Pattern.compile("<br[^>]*>");
    private final static Pattern wbrPattern  = Pattern.compile("<wbr[^>]*>");

    /*
     * return tmplinks.toArray(new String[tmplinks.size()]); }
     *
     * /* parses data for available links and returns a string array which does not contain any duplicates
     */
    public static Collection<String> getHttpLinksIntern(final String content, final String baseURLString, final HtmlParserResultSet results) {
        if (content == null || content.length() == 0) {
            return null;
        }
        HtmlParserCharSequence data = new HtmlParserCharSequence(content);
        data = data.trim();
        if (data.length() == 0) {
            return null;
        }
        /*
         * replace urlencoded br tags, so we can find all links separated by those
         */
        /* find a better solution for this html codings */
        data = data.replaceAll(HTMLParser.ltPattern, ">");
        data = data.replaceAll(HTMLParser.gtPattern, "<");
        data = data.replaceAll(HTMLParser.ampPattern, "&");
        data = data.replaceAll(HTMLParser.quotPattern, "\"");
        /* place all replaces here that separates links */
        /* replace <br> tags with space so we we can separate the links */
        /* we replace the complete br tag with a newline */
        data = data.replaceAll(HTMLParser.brPattern, "\r\n");
        /* remove word breaks */
        data = data.replaceAll(HTMLParser.wbrPattern, "");
        /* remove HTML Tags */
        data = data.replaceAll(Pattern.compile("</?(i|b|u|s)>"), "");
        /*
         * remove all span because they can break url parsing (eg when google-code-prettify is used)
         */
        // not needed here because our filter below will take care of them
        // data = data.replaceAll("(?i)<span.*?>", "");
        // data = data.replaceAll("(?i)</span.*?>", "");
        data = data.replaceAll(Pattern.compile("(?s)\\[(url|link)\\](.*?)\\[/(\\2)\\]"), "<$2>");
        final HtmlParserResultSet resultSet;
        if (results != null) {
            resultSet = results;
        } else {
            resultSet = new HtmlParserResultSet();
        }
        if (baseURLString != null && HTMLParser.isSupportedProtocol(baseURLString)) {
            resultSet.setBaseURL(new HtmlParserCharSequence(baseURLString));
        }
        HTMLParser._getHttpLinksWalker(HTMLParser.next(null, data), resultSet, null, null);
        data = null;
        /* we don't want baseurl to be included in result set */
        if (Boolean.TRUE.equals(resultSet.isSkipBaseURL()) && resultSet.getBaseURL() != null) {
            resultSet.remove(resultSet.getBaseURL());
        }
        // System.out.println("Walker:" + results.getWalkerCounter() + "|DeepWalker:" + results.getDeepWalkerCounter() + "|Finder:" +
        // results.getFinderCounter() + "|Found:" + results.size());
        final Collection<String> ret = resultSet.exportResults();
        if (ret.size() == 0) {
            return null;
        } else {
            return ret;
        }
    }

    /**
     * Gibt alle Hidden fields als hasMap zurÃ¼ck
     *
     * @param data
     * @return hasmap mit allen hidden fields variablen
     */
    public static HashMap<String, String> getInputHiddenFields(final String data) {
        final Pattern intput1 = Pattern.compile("(?s)<\\s*input([^>]*type\\s*=\\s*['\"]?hidden['\"]?[^>]*?)[/]?>", Pattern.CASE_INSENSITIVE);
        final Pattern intput2 = Pattern.compile("name\\s*=\\s*['\"]([^'\"]*?)['\"]", Pattern.CASE_INSENSITIVE);
        final Pattern intput3 = Pattern.compile("value\\s*=\\s*['\"]([^'\"]*?)['\"]", Pattern.CASE_INSENSITIVE);
        final Pattern intput4 = Pattern.compile("name\\s*=\\s*([^\\s]*)", Pattern.CASE_INSENSITIVE);
        final Pattern intput5 = Pattern.compile("value\\s*=\\s*([^\\s]*)", Pattern.CASE_INSENSITIVE);
        final Matcher matcher1 = intput1.matcher(data);
        Matcher matcher2;
        Matcher matcher3;
        Matcher matcher4;
        Matcher matcher5;
        final HashMap<String, String> ret = new HashMap<String, String>();
        boolean iscompl;
        while (matcher1.find()) {
            matcher2 = intput2.matcher(matcher1.group(1) + " ");
            matcher3 = intput3.matcher(matcher1.group(1) + " ");
            matcher4 = intput4.matcher(matcher1.group(1) + " ");
            matcher5 = intput5.matcher(matcher1.group(1) + " ");
            iscompl = false;
            String key, value;
            key = value = null;
            if (matcher2.find()) {
                iscompl = true;
                key = matcher2.group(1);
            } else if (matcher4.find()) {
                iscompl = true;
                key = matcher4.group(1);
            }
            if (matcher3.find() && iscompl) {
                value = matcher3.group(1);
            } else if (matcher5.find() && iscompl) {
                value = matcher5.group(1);
            } else {
                iscompl = false;
            }
            ret.put(key, value);
        }
        return ret;
    }

    private static HtmlParserCharSequence getProtocol(final HtmlParserCharSequence url) {
        HtmlParserCharSequence ret = url != null ? url.group(1, HTMLParser.LINKPROTOCOL) : null;
        if (ret != null) {
            if (ret.contains("\\x3A)") || ret.contains("\\x2F")) {
                ret = HTMLParser.unicodeDecode(ret);
            } else if (ret.contains("\\u003A)") || ret.contains("\\u002F")) {
                ret = HTMLParser.unicodeDecode(ret);
            }
        }
        return ret;
    }

    private static HtmlParserCharSequence unicodeDecode(final CharSequence cs) {
        final CharSequence decoded = Encoding.unicodeDecode(cs, true);
        if (!CharSequenceUtils.contentEquals(decoded, cs)) {
            return new HtmlParserCharSequence(decoded);
        } else if (cs instanceof HtmlParserCharSequence) {
            return (HtmlParserCharSequence) cs;
        } else {
            return new HtmlParserCharSequence(cs);
        }
    }

    private static HtmlParserCharSequence htmlDecode(final CharSequence cs) {
        final CharSequence decoded = Encoding.htmlOnlyDecode(cs.toString(), true);
        if (!CharSequenceUtils.contentEquals(decoded, cs)) {
            return new HtmlParserCharSequence(decoded);
        } else if (cs instanceof HtmlParserCharSequence) {
            return (HtmlParserCharSequence) cs;
        } else {
            return new HtmlParserCharSequence(cs);
        }
    }

    public static String getProtocol(final String url) {
        if (url != null) {
            final HtmlParserCharSequence ret = HTMLParser.getProtocol(new HtmlParserCharSequence(url));
            if (ret != null) {
                return ret.toString();
            }
        }
        return null;
    }

    public static boolean isSupportedProtocol(final String url) {
        if (url != null) {
            return HTMLParser.getProtocol(new HtmlParserCharSequence(url)) != null;
        } else {
            return false;
        }
    }

    /**
     * @author olimex FÃ¼gt Map als String mit Trennzeichen zusammen TODO: auslagern
     * @param map
     *            Map
     * @param delPair
     *            Trennzeichen zwischen Key und Value
     * @param delMap
     *            Trennzeichen zwischen Map-EintrÃ¤gen
     * @return Key-value pairs
     */
    public static String joinMap(final Map<String, String> map, final String delPair, final String delMap) {
        final StringBuilder buffer = new StringBuilder();
        boolean first = true;
        for (final Map.Entry<String, String> entry : map.entrySet()) {
            if (first) {
                first = false;
            } else {
                buffer.append(delMap);
            }
            buffer.append(entry.getKey());
            buffer.append(delPair);
            buffer.append(entry.getValue());
        }
        return buffer.toString();
    }

    private static HtmlParserCharSequence parseLocation(final HtmlParserResultSet results, final HtmlParserCharSequence baseURL, final HtmlParserCharSequence loc) {
        return HTMLParser.parseLocation(results, baseURL, loc, false);
    }

    private static final Pattern parseLocationPatternSchemeHostLoc = Pattern.compile("^:\\d+/.*");

    private static HtmlParserCharSequence parseLocation(final HtmlParserResultSet results, final HtmlParserCharSequence baseURL, final HtmlParserCharSequence loc, final boolean schemeAndHostOnlyMode) {
        if (HTMLParser.getProtocol(loc) != null) {
            /* full location */
            return loc;
        } else {
            final HtmlParserCharSequence baseProtocol = HTMLParser.getProtocol(baseURL);
            if (loc == null || baseURL == null || baseProtocol == null || loc.length() == 0) {
                return null;
            } else if (loc.matches(HTMLParser.parseLocationPatternSchemeHostLoc)) {
                /* scheme + host + loc */
                final HtmlParserCharSequence root = baseURL.group(1, HTMLParser.mergePattern_Root);
                return new HtmlParserCharSequence(new ConcatCharSequence(root, loc));
            } else if (loc.startsWith("//")) {
                /* scheme + location */
                return new HtmlParserCharSequence(new ConcatCharSequence(baseProtocol.subSequence(0, baseProtocol.length() - 2), loc));
            } else if (schemeAndHostOnlyMode) {
                return null;
            } else if (loc.startsWith("/")) {
                /* absolute path */
                final HtmlParserCharSequence root = baseURL.group(1, HTMLParser.mergePattern_Root);
                if (root != null) {
                    /* append location to root */
                    return new HtmlParserCharSequence(new ConcatCharSequence(root, loc));
                }
            } else if (loc.startsWith("./")) {
                /* relative path */
                final HtmlParserCharSequence path = baseURL.group(1, HTMLParser.mergePattern_Directory);
                if (path != null) {
                    /* relative path possible */
                    return new HtmlParserCharSequence(new ConcatCharSequence(path, loc.subSequence(2)));
                } else {
                    /* no relative path possible, retry with absolute path */
                    return HTMLParser.parseLocation(results, baseURL, loc.subSequence(1));
                }
            } else if (loc.startsWith("../")) {
                /* upper directory path */
                /* TODO: optimize to use ConcatCharSequence */
                try {
                    final String url = URLHelper.parseLocation(new URL(baseURL.toURL()), loc.toURL());
                    return new HtmlParserCharSequence(url);
                } catch (final Throwable e) {
                    HTMLParser.logThrowable(results, e);
                }
            } else if (loc.startsWith("#")) {
                /* anchor */
                /* ignore anchor */
                return null;
            } else if (loc.startsWith("?")) {
                /* query */
                final HtmlParserCharSequence path = baseURL.group(1, HTMLParser.mergePattern_Path);
                if (path != null) {
                    /* append query to path */
                    return new HtmlParserCharSequence(new ConcatCharSequence(path, loc));
                } else {
                    final HtmlParserCharSequence root = baseURL.group(1, HTMLParser.mergePattern_Root);
                    if (root != null) {
                        /* append query to root */
                        return new HtmlParserCharSequence(new ConcatCharSequence(root, "/", loc));
                    }
                }
            } else if (loc.startsWith("&")) {
                /* additional query */
                /* TODO: optimize to use ConcatCharSequence */
                try {
                    final String url = URLHelper.parseLocation(new URL(baseURL.toURL()), loc.toURL());
                    return new HtmlParserCharSequence(url);
                } catch (final Throwable e) {
                    HTMLParser.logThrowable(results, e);
                }
            } else {
                /* relative path relative to baseURL */
                final HtmlParserCharSequence path = baseURL.group(1, HTMLParser.mergePattern_Directory);
                if (path != null) {
                    /* relative to current path */
                    return new HtmlParserCharSequence(new ConcatCharSequence(path, loc));
                } else {
                    final HtmlParserCharSequence root = baseURL.group(1, HTMLParser.mergePattern_Root);
                    if (root != null) {
                        /* relative to root */
                        return new HtmlParserCharSequence(new ConcatCharSequence(root, "/", loc));
                    }
                }
            }
            return null;
        }
    }
}
