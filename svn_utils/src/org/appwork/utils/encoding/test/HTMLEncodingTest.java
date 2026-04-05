/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *         This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *         The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *         These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *         Some parts of [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *         to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *         If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *         If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *         If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *         Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *         If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *         "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *         If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.encoding.test;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.HTMLEncoding;

/**
 * Regression and coverage tests for {@link HTMLEncoding}.
 */
public class HTMLEncodingTest extends AWTest {

    public static void main(final String[] args) throws Exception {
        run();
    }

    private static void assertEq(final String message, final String expected, final String actual) {
        if (!StringUtils.equals(expected, actual)) {
            throw new WTFException((message != null ? message + ": " : "") + "expected <" + expected + "> actual <" + actual + ">");
        }
    }

    @Override
    public void runTest() throws Exception {
        testEscapeNullAndEmpty();
        testEscapeMarkupAndQuotes();
        testEscapeLineBreaksAndTitle();
        testEscapeUnicodePassthrough();
        testUnescapeNullAndEmpty();
        testUnescapeNamedEntities();
        testUnescapeNumericEntities();
        testUnescapeSequencesAndCase();
        testUnescapeInvalidAndLoneAmpersand();
        testUnescapeTrim();
        testRoundtripAndDoubleEscape();
    }

    private static void testEscapeNullAndEmpty() {
        assertEq("escapeTitle null", "", HTMLEncoding.escapeTitle(null));
        assertEq("escapeText null", "", HTMLEncoding.escapeText((String) null));
        assertEq("escapeText empty", "", HTMLEncoding.escapeText(""));
        assertEq("escapeAttr empty", "", HTMLEncoding.escapeAttribute(""));
    }

    private static void testEscapeMarkupAndQuotes() {
        assertEq("markup", "&amp;&lt;&gt;&quot;&#39;", HTMLEncoding.escapeAttribute("&<>\"'"));
        assertEq("no double-encode amp", "&amp;amp;", HTMLEncoding.escapeAttribute("&amp;"));
        assertEq("angle only", "&lt;tag&gt;", HTMLEncoding.escapeText("<tag>", false));
    }

    private static void testEscapeLineBreaksAndTitle() {
        assertEq("br lf cr", "a<br>b<br>c", HTMLEncoding.escapeText("a\nb\rc"));
        assertEq("attr spaces", "a b c", HTMLEncoding.escapeAttribute("a\nb\rc"));
        assertEq("title flatten", "a b c", HTMLEncoding.escapeTitle("a\nb\rc"));
        assertEq("title newline run one space", "x y", HTMLEncoding.escapeTitle("x\n\ry"));
        assertEq("title crlf one space", "a b", HTMLEncoding.escapeTitle("a\r\nb"));
        assertEq("title double newline one space", "a b", HTMLEncoding.escapeTitle("a\n\nb"));
    }

    private static void testEscapeUnicodePassthrough() {
        final String umlauts = "Fahrradstra\u00DFe \u00FCber \u00F6ffentlich";
        assertEq("unicode unchanged except special", umlauts, HTMLEncoding.escapeText(umlauts, false));
        assertEq("unicode plus amp", "caf\u00E9 &amp; tea", HTMLEncoding.escapeText("caf\u00E9 & tea", false));
    }

    private static void testUnescapeNullAndEmpty() {
        if (HTMLEncoding.unescape(null) != null) {
            throw new WTFException("unescape null should return null");
        }
        assertEq("unescape empty", "", HTMLEncoding.unescape(""));
    }

    private static void testUnescapeNamedEntities() {
        assertEq("euro", "\u20AC", HTMLEncoding.unescape("&euro;"));
        assertEq("copy", "\u00A9", HTMLEncoding.unescape("&copy;"));
        assertEq("nbsp", "\u00A0", HTMLEncoding.unescape("&nbsp;"));
        assertEq("mdash", "\u2014", HTMLEncoding.unescape("&mdash;"));
        assertEq("ldquo", "\u201C", HTMLEncoding.unescape("&ldquo;"));
        assertEq("sum", "\u2211", HTMLEncoding.unescape("&sum;"));
        assertEq("infin", "\u221E", HTMLEncoding.unescape("&infin;"));
        assertEq("alpha", "\u03B1", HTMLEncoding.unescape("&alpha;"));
        assertEq("lt gt quot", "<>\"", HTMLEncoding.unescape("&lt;&gt;&quot;"));
        assertEq("apos", "'", HTMLEncoding.unescape("&apos;"));
        assertEq("rsquo", "\u2019", HTMLEncoding.unescape("&rsquo;"));
        assertEq("chain amp", "a & b", HTMLEncoding.unescape("a &amp; b"));
        assertEq("two entities", "A\u20AC", HTMLEncoding.unescape("A&euro;"));
    }

    private static void testUnescapeNumericEntities() {
        assertEq("dec euro", "\u20AC", HTMLEncoding.unescape("&#8364;"));
        assertEq("hex euro", "\u20AC", HTMLEncoding.unescape("&#x20AC;"));
        assertEq("hex A", "A", HTMLEncoding.unescape("&#x41;"));
        assertEq("dec digit", "0", HTMLEncoding.unescape("&#48;"));
        assertEq("leading zeros dec", "A", HTMLEncoding.unescape("&#00065;"));
        assertEq("no semi dec eof", "\u20AC", HTMLEncoding.unescape("&#8364"));
        assertEq("no semi hex eof", "\u20AC", HTMLEncoding.unescape("&#x20AC"));
        assertEq("no semi named eof", "&", HTMLEncoding.unescape("&amp"));
        assertEq("no semi lt eof", "<", HTMLEncoding.unescape("&lt"));
        assertEq("no semi two amps eof", "&amp", HTMLEncoding.unescape("&amp;amp"));
        assertEq("supplementary dec", "\uD83D\uDE00", HTMLEncoding.unescape("&#128512;"));
        assertEq("supplementary hex plane", "\uD800\uDC00", HTMLEncoding.unescape("&#x10000;"));
        assertEq("max plane", "\uDBFF\uDFFF", HTMLEncoding.unescape("&#x10FFFF;"));
    }

    private static void testUnescapeSequencesAndCase() {
        assertEq("empty entity &;", "&;", HTMLEncoding.unescape("&;"));
        assertEq("space after amp not entity", "& x", HTMLEncoding.unescape("& x"));
        assertEq("unknown named", "&bogus;", HTMLEncoding.unescape("&bogus;"));
        assertEq("unknown named tail", "&bogus;x", HTMLEncoding.unescape("&bogus;x"));
        assertEq("uppercase named not matched", "&LT;", HTMLEncoding.unescape("&LT;"));
        assertEq("mixed line", "1<2 & 3\u20AC", HTMLEncoding.unescape("1&lt;2 &amp; 3&euro;"));
    }

    private static void testUnescapeInvalidAndLoneAmpersand() {
        assertEq("lone amp", "price & weight", HTMLEncoding.unescape("price & weight"));
        assertEq("invalid dec", "&#xx;", HTMLEncoding.unescape("&#xx;"));
        assertEq("invalid hex empty", "&#x;", HTMLEncoding.unescape("&#x;"));
        assertEq("malformed dec body", "&#abc;", HTMLEncoding.unescape("&#abc;"));
    }

    private static void testUnescapeTrim() {
        assertEq("trim true", "x", HTMLEncoding.unescape(" x ", true));
        assertEq("trim false inner spaces", " x y ", HTMLEncoding.unescape(" x y ", false));
        assertEq("trim false no trim", "x", HTMLEncoding.unescape("x", false));
    }

    private static void testRoundtripAndDoubleEscape() {
        final String plain = "foo & bar <tag> \"q\" 's'";
        assertEq("roundtrip plain no br", plain, HTMLEncoding.unescape(HTMLEncoding.escapeText(plain, false)));

        final String safeAscii = "Hello-123_test";
        assertEq("roundtrip ascii", safeAscii, HTMLEncoding.unescape(HTMLEncoding.escapeAttribute(safeAscii)));

        final String once = HTMLEncoding.escapeAttribute("&<>\"'");
        final String twice = HTMLEncoding.escapeAttribute(once);
        if (StringUtils.equals(once, twice)) {
            throw new WTFException("double escape should grow string");
        }
        if (twice.indexOf("&amp;amp;") < 0) {
            throw new WTFException("double escape should contain &amp;amp;");
        }
        assertEq("double-escaped then single-pass unescaped", "&amp;&lt;&gt;&quot;&#39;", HTMLEncoding.unescape(twice));

        final String withBr = "a\nb";
        final String escBr = HTMLEncoding.escapeText(withBr);
        assertEq("br preserved in unescape", "a<br>b", HTMLEncoding.unescape(escBr));
    }
}
