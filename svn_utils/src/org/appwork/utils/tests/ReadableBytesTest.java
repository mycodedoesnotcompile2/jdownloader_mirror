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
package org.appwork.utils.tests;

import java.util.LinkedHashMap;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.ReadableBytes;
import org.appwork.utils.ReadableBytes.ReadableBytesParseException;

/**
 * @author thomas
 * @date 20.09.2023
 *
 */
@TestDependency({ "org.appwork.utils.ReadableBytes" })
public class ReadableBytesTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        // Test basic byte units (1000 factor)
        testBasicBytes();

        // Test binary units (1024 factor - KiB, MiB, GiB, TiB)
        testBinaryBytes();

        // Test decimal units (1000 factor - KB, MB, GB, TB)
        testDecimalBytes();

        // Test floating point numbers with comma (German format)
        testFloatingPointComma();

        // Test floating point numbers with dot (English format)
        testFloatingPointDot();

        // Test multiple units in one string
        testMultipleUnits();

        // Test negative values
        testNegativeValues();

        // Test formatting output
        testFormatting();

        // Test edge cases
        testEdgeCases();

        // Test parsing exceptions
        testParseExceptions();

        // Test fromBytes factory methods
        testFromBytes();

        // Test units() method
        testUnits();

        // Test ceil() method
        testCeil();
    }

    private void testBasicBytes() throws Exception {
        // Test bytes (B)
        assertEquals(ReadableBytes.parse("0B").getBytes(), 0L);
        assertEquals(ReadableBytes.parse("1B").getBytes(), 1L);
        assertEquals(ReadableBytes.parse("100B").getBytes(), 100L);
        assertEquals(ReadableBytes.parse("1024B").getBytes(), 1024L);
        assertFalse(ReadableBytes.parse("1B").isKibi());
        assertFalse(ReadableBytes.parse("1b").isKibi());
    }

    private void testBinaryBytes() throws Exception {
        // Test KiB (1024 bytes)
        assertEquals(ReadableBytes.parse("1KiB").getBytes(), 1024L);
        assertEquals(ReadableBytes.parse("1kib").getBytes(), 1024L);
        assertEquals(ReadableBytes.parse("2KiB").getBytes(), 2 * 1024L);
        assertTrue(ReadableBytes.parse("1KiB").isKibi());
        assertTrue(ReadableBytes.parse("1kib").isKibi());

        // Test MiB (1024^2 bytes)
        assertEquals(ReadableBytes.parse("1MiB").getBytes(), 1024L * 1024L);
        assertEquals(ReadableBytes.parse("1mib").getBytes(), 1024L * 1024L);
        assertEquals(ReadableBytes.parse("2MiB").getBytes(), 2L * 1024L * 1024L);
        assertTrue(ReadableBytes.parse("1MiB").isKibi());

        // Test GiB (1024^3 bytes)
        assertEquals(ReadableBytes.parse("1GiB").getBytes(), 1024L * 1024L * 1024L);
        assertEquals(ReadableBytes.parse("1gib").getBytes(), 1024L * 1024L * 1024L);
        assertEquals(ReadableBytes.parse("2GiB").getBytes(), 2L * 1024L * 1024L * 1024L);
        assertTrue(ReadableBytes.parse("1GiB").isKibi());

        // Test TiB (1024^4 bytes)
        assertEquals(ReadableBytes.parse("1TiB").getBytes(), 1024L * 1024L * 1024L * 1024L);
        assertEquals(ReadableBytes.parse("1tib").getBytes(), 1024L * 1024L * 1024L * 1024L);
        assertEquals(ReadableBytes.parse("2TiB").getBytes(), 2L * 1024L * 1024L * 1024L * 1024L);
        assertTrue(ReadableBytes.parse("1TiB").isKibi());
    }

    private void testDecimalBytes() throws Exception {
        // Test KB (1000 bytes)
        assertEquals(ReadableBytes.parse("1KB").getBytes(), 1000L);
        assertEquals(ReadableBytes.parse("1kb").getBytes(), 1000L);
        assertEquals(ReadableBytes.parse("2KB").getBytes(), 2000L);
        assertFalse(ReadableBytes.parse("1KB").isKibi());

        // Test MB (1000^2 bytes)
        assertEquals(ReadableBytes.parse("1MB").getBytes(), 1000L * 1000L);
        assertEquals(ReadableBytes.parse("1mb").getBytes(), 1000L * 1000L);
        assertEquals(ReadableBytes.parse("2MB").getBytes(), 2L * 1000L * 1000L);
        assertFalse(ReadableBytes.parse("1MB").isKibi());

        // Test GB (1000^3 bytes)
        assertEquals(ReadableBytes.parse("1GB").getBytes(), 1000L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("1gb").getBytes(), 1000L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("2GB").getBytes(), 2L * 1000L * 1000L * 1000L);
        assertFalse(ReadableBytes.parse("1GB").isKibi());

        // Test TB (1000^4 bytes)
        assertEquals(ReadableBytes.parse("1TB").getBytes(), 1000L * 1000L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("1tb").getBytes(), 1000L * 1000L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("2TB").getBytes(), 2L * 1000L * 1000L * 1000L * 1000L);
        assertFalse(ReadableBytes.parse("1TB").isKibi());
    }

    private void testFloatingPointComma() throws Exception {
        // Test German decimal separator (comma) with decimal units
        assertEquals(ReadableBytes.parse("1,5KB").getBytes(), 1500L);
        assertEquals(ReadableBytes.parse("1,5MB").getBytes(), 1500L * 1000L);
        assertEquals(ReadableBytes.parse("1,5GB").getBytes(), 1500L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("2,5MB").getBytes(), 2500L * 1000L);
        assertEquals(ReadableBytes.parse("0,5GB").getBytes(), 500L * 1000L * 1000L);

        // Test German decimal separator (comma) with binary units
        assertEquals(ReadableBytes.parse("1,5KiB").getBytes(), (long) (1.5 * 1024));
        assertEquals(ReadableBytes.parse("1,5MiB").getBytes(), (long) (1.5 * 1024 * 1024));
        assertEquals(ReadableBytes.parse("1,5GiB").getBytes(), (long) (1.5 * 1024 * 1024 * 1024));
        assertEquals(ReadableBytes.parse("2,5MiB").getBytes(), (long) (2.5 * 1024 * 1024));

        // Test with spaces
        assertEquals(ReadableBytes.parse("1,5 MB").getBytes(), 1500L * 1000L);
        assertEquals(ReadableBytes.parse("1,5 MiB").getBytes(), (long) (1.5 * 1024 * 1024));
    }

    private void testFloatingPointDot() throws Exception {
        // Test English decimal separator (dot) with decimal units
        assertEquals(ReadableBytes.parse("1.5KB").getBytes(), 1500L);
        assertEquals(ReadableBytes.parse("1.5MB").getBytes(), 1500L * 1000L);
        assertEquals(ReadableBytes.parse("1.5GB").getBytes(), 1500L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("2.5MB").getBytes(), 2500L * 1000L);
        assertEquals(ReadableBytes.parse("0.5GB").getBytes(), 500L * 1000L * 1000L);

        // Test English decimal separator (dot) with binary units
        assertEquals(ReadableBytes.parse("1.5KiB").getBytes(), (long) (1.5 * 1024));
        assertEquals(ReadableBytes.parse("1.5MiB").getBytes(), (long) (1.5 * 1024 * 1024));
        assertEquals(ReadableBytes.parse("1.5GiB").getBytes(), (long) (1.5 * 1024 * 1024 * 1024));
        assertEquals(ReadableBytes.parse("2.5MiB").getBytes(), (long) (2.5 * 1024 * 1024));

        // Test with spaces
        assertEquals(ReadableBytes.parse("1.5 MB").getBytes(), 1500L * 1000L);
        assertEquals(ReadableBytes.parse("1.5 MiB").getBytes(), (long) (1.5 * 1024 * 1024));
    }

    private void testMultipleUnits() throws Exception {
        // Test multiple units in one string
        assertEquals(ReadableBytes.parse("1MB 500KB").getBytes(), 1000L * 1000L + 500L * 1000L);
        assertEquals(ReadableBytes.parse("1,5 MB 500kb").getBytes(), 1500L * 1000L + 500L * 1000L);
        assertEquals(ReadableBytes.parse("1MiB 512KiB").getBytes(), 1024L * 1024L + 512L * 1024L);
        assertEquals(ReadableBytes.parse("2GB 500MB 250KB").getBytes(), 2L * 1000L * 1000L * 1000L + 500L * 1000L * 1000L + 250L * 1000L);
        assertEquals(ReadableBytes.parse("1TB 500GB 250MB").getBytes(), 1000L * 1000L * 1000L * 1000L + 500L * 1000L * 1000L * 1000L + 250L * 1000L * 1000L);

        // Test mixed binary and decimal (should detect kibi if any binary unit is present)
        assertEquals(ReadableBytes.parse("1MiB 500KB").getBytes(), 1024L * 1024L + 500L * 1000L);
        assertTrue(ReadableBytes.parse("1MiB 500KB").isKibi());
    }

    private void testNegativeValues() throws Exception {
        // Test negative values
        assertEquals(ReadableBytes.parse("-1B").getBytes(), -1L);
        assertEquals(ReadableBytes.parse("-1KB").getBytes(), -1000L);
        assertEquals(ReadableBytes.parse("-1MB").getBytes(), -1000L * 1000L);
        assertEquals(ReadableBytes.parse("-1KiB").getBytes(), -1024L);
        assertEquals(ReadableBytes.parse("-1MiB").getBytes(), -1024L * 1024L);
        assertEquals(ReadableBytes.parse("-1,5MB").getBytes(), -1500L * 1000L);
        assertEquals(ReadableBytes.parse("-1.5MB").getBytes(), -1500L * 1000L);
    }

    private void testFormatting() throws Exception {
        // Test formatting decimal units
        assertEquals(new ReadableBytes(1000L, false).format(), "1KB");
        assertEquals(new ReadableBytes(2000000L, false).format(), "2MB");
        assertEquals(new ReadableBytes(2000000000L, false).format(), "2GB");
        assertEquals(new ReadableBytes(2000000000000L, false).format(), "2TB");

        // Test formatting binary units
        assertEquals(new ReadableBytes(1024L, true).format(), "1KiB");
        assertEquals(new ReadableBytes(2L * 1024L * 1024L, true).format(), "2MiB");
        assertEquals(new ReadableBytes(2L * 1024L * 1024L * 1024L, true).format(), "2GiB");
        assertEquals(new ReadableBytes(2L * 1024L * 1024L * 1024L * 1024L, true).format(), "2TiB");

        // Test formatting with multiple units
        assertEquals(new ReadableBytes(1024L * 1024L + 512L * 1024L, true).format(), "1MiB512KiB");
        assertEquals(new ReadableBytes(1000L * 1000L + 500L * 1000L, false).format(), "1MB500KB");

        // Test zero
        assertEquals(new ReadableBytes(0L, false).format(), "0B");
        assertEquals(new ReadableBytes(0L, true).format(), "0B");
        assertEquals(ReadableBytes.ZERO.format(), "0B");

        // Test negative formatting
        assertEquals(new ReadableBytes(-1000L, false).format(), "-1KB");
        assertEquals(new ReadableBytes(-1024L, true).format(), "-1KiB");
    }

    private void testEdgeCases() throws Exception {
        // Test zero
        assertEquals(ReadableBytes.ZERO.getBytes(), 0L);
        assertEquals(new ReadableBytes(0L).getBytes(), 0L);
        assertEquals(new ReadableBytes(0L, false).getBytes(), 0L);
        assertEquals(new ReadableBytes(0L, true).getBytes(), 0L);

        // Test very large values
        assertEquals(ReadableBytes.parse("1TB").getBytes(), 1000L * 1000L * 1000L * 1000L);
        assertEquals(ReadableBytes.parse("1TiB").getBytes(), 1024L * 1024L * 1024L * 1024L);

        // Test case insensitivity
        assertEquals(ReadableBytes.parse("1kb").getBytes(), ReadableBytes.parse("1KB").getBytes());
        assertEquals(ReadableBytes.parse("1mb").getBytes(), ReadableBytes.parse("1MB").getBytes());
        assertEquals(ReadableBytes.parse("1gb").getBytes(), ReadableBytes.parse("1GB").getBytes());
        assertEquals(ReadableBytes.parse("1tb").getBytes(), ReadableBytes.parse("1TB").getBytes());
        assertEquals(ReadableBytes.parse("1kib").getBytes(), ReadableBytes.parse("1KiB").getBytes());
        assertEquals(ReadableBytes.parse("1mib").getBytes(), ReadableBytes.parse("1MiB").getBytes());
        assertEquals(ReadableBytes.parse("1gib").getBytes(), ReadableBytes.parse("1GiB").getBytes());
        assertEquals(ReadableBytes.parse("1tib").getBytes(), ReadableBytes.parse("1TiB").getBytes());

        // Test whitespace handling
        assertEquals(ReadableBytes.parse("1 MB").getBytes(), ReadableBytes.parse("1MB").getBytes());
        assertEquals(ReadableBytes.parse("1  MB").getBytes(), ReadableBytes.parse("1MB").getBytes());
        assertEquals(ReadableBytes.parse("1\tMB").getBytes(), ReadableBytes.parse("1MB").getBytes());
    }

    private void testParseExceptions() throws Exception {
        // Test invalid unit
        try {
            ReadableBytes.parse("1Byte");
            throw new Exception("Expected ReadableBytesParseException for invalid unit");
        } catch (ReadableBytesParseException e) {
            // Expected
        }

        // Test invalid format
        try {
            ReadableBytes.parse("invalid");
            throw new Exception("Expected ReadableBytesParseException for invalid format");
        } catch (ReadableBytesParseException e) {
            // Expected
        }

        // Test empty string
        try {
            ReadableBytes.parse("");
            throw new Exception("Expected ReadableBytesParseException for empty string");
        } catch (ReadableBytesParseException e) {
            // Expected
        }
        // Test empty string
        try {
            ReadableBytes.parse("-");
            throw new Exception("Expected ReadableBytesParseException for - string");
        } catch (ReadableBytesParseException e) {
            // Expected
        }
        // Test only number without unit
        try {
            ReadableBytes.parse("123");
            throw new Exception("Expected ReadableBytesParseException for number without unit");
        } catch (ReadableBytesParseException e) {
            // Expected
        }
    }

    private void testFromBytes() throws Exception {
        // Test fromBytes with kibi flag
        ReadableBytes rb1 = ReadableBytes.fromBytes(1024L, true);
        assertEquals(rb1.getBytes(), 1024L);
        assertTrue(rb1.isKibi());

        ReadableBytes rb2 = ReadableBytes.fromBytes(1000L, false);
        assertEquals(rb2.getBytes(), 1000L);
        assertFalse(rb2.isKibi());

        // Test fromBytes without kibi flag (defaults to false)
        ReadableBytes rb3 = ReadableBytes.fromBytes(1000L);
        assertEquals(rb3.getBytes(), 1000L);
        assertFalse(rb3.isKibi());

        // Test constructor
        ReadableBytes rb4 = new ReadableBytes(1024L, true);
        assertEquals(rb4.getBytes(), 1024L);
        assertTrue(rb4.isKibi());

        ReadableBytes rb5 = new ReadableBytes(1000L);
        assertEquals(rb5.getBytes(), 1000L);
        assertTrue(rb5.isKibi()); // Default constructor uses kibi=true
    }

    private void testUnits() throws Exception {
        // Test units() method with decimal units
        ReadableBytes rb1 = new ReadableBytes(2000000L, false);
        LinkedHashMap<ReadableBytes.Unit, Long> units1 = rb1.units();
        assertEquals(units1.size(), 1);
        assertEquals(units1.get(ReadableBytes.Unit.MB), Long.valueOf(2L));

        // Test units() method with binary units
        ReadableBytes rb2 = new ReadableBytes(2L * 1024L * 1024L, true);
        LinkedHashMap<ReadableBytes.Unit, Long> units2 = rb2.units();
        assertEquals(units2.size(), 1);
        assertEquals(units2.get(ReadableBytes.Unit.MB), Long.valueOf(2L));

        // Test units() with multiple units (needs to be > factor, not >=)
        ReadableBytes rb3 = new ReadableBytes(2L * 1024L * 1024L + 512L * 1024L, true);
        LinkedHashMap<ReadableBytes.Unit, Long> units3 = rb3.units();
        assertEquals(units3.size(), 2);
        assertEquals(units3.get(ReadableBytes.Unit.MB), Long.valueOf(2L));
        assertEquals(units3.get(ReadableBytes.Unit.KB), Long.valueOf(512L));

        // Test units() with zero
        ReadableBytes rb4 = new ReadableBytes(0L, false);
        LinkedHashMap<ReadableBytes.Unit, Long> units4 = rb4.units();
        assertEquals(units4.size(), 1);
        assertEquals(units4.get(ReadableBytes.Unit.B), Long.valueOf(0L));

        // Test units() with single unit exactly at threshold (should not appear due to > check)
        ReadableBytes rb5 = new ReadableBytes(1000L * 1000L, false);
        LinkedHashMap<ReadableBytes.Unit, Long> units5 = rb5.units();
        // 1MB exactly, but units() uses > so it won't include MB, will break down to KB
        assertEquals(units5.size(), 1);
        assertEquals(units5.get(ReadableBytes.Unit.KB), Long.valueOf(1000L));
    }

    private void testCeil() throws Exception {
        // Test ceil() method with decimal units
        ReadableBytes rb1 = new ReadableBytes(1500L, false);
        ReadableBytes ceil1 = rb1.ceil(1L, ReadableBytes.Unit.KB);
        assertEquals(ceil1.getBytes(), 2000L); // Ceiled to 2KB (1 * 2 * 1000)
        assertFalse(ceil1.isKibi());

        ReadableBytes rb2 = new ReadableBytes(2500L, false);
        ReadableBytes ceil2 = rb2.ceil(1L, ReadableBytes.Unit.KB);
        assertEquals(ceil2.getBytes(), 3000L); // Ceiled to 3KB

        // Test ceil() method with binary units
        ReadableBytes rb3 = new ReadableBytes(1024L * 1024L + 100L, true);
        ReadableBytes ceil3 = rb3.ceil(1L, ReadableBytes.Unit.MB);
        assertEquals(ceil3.getBytes(), 2L * 1024L * 1024L); // Ceiled to 2MiB
        assertTrue(ceil3.isKibi());

        // Test ceil() when already exact
        ReadableBytes rb4 = new ReadableBytes(2000L, false);
        ReadableBytes ceil4 = rb4.ceil(1L, ReadableBytes.Unit.KB);
        assertEquals(ceil4.getBytes(), 2000L); // Already exact (2KB), should return same
        assertEquals(ceil4.isKibi(), rb4.isKibi()); // Should preserve kibi flag

        // Test ceil() with amount parameter
        ReadableBytes rb5 = new ReadableBytes(1500L, false);
        ReadableBytes ceil5 = rb5.ceil(2L, ReadableBytes.Unit.KB);
        // d = (1500 / 2.0) / 1000 = 0.75, ceil(0.75) = 1, newBytes = 1 * 2 * 1000 = 2000
        assertEquals(ceil5.getBytes(), 2000L);

        ReadableBytes rb6 = new ReadableBytes(3500L, false);
        ReadableBytes ceil6 = rb6.ceil(2L, ReadableBytes.Unit.KB);
        // d = (3500 / 2.0) / 1000 = 1.75, ceil(1.75) = 2, newBytes = 2 * 2 * 1000 = 4000
        assertEquals(ceil6.getBytes(), 4000L);
    }
}
