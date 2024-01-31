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
package org.appwork.storage.flexijson.mapper.tests;

import java.text.ParseException;

import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.ReadableBytes;
import org.appwork.utils.ReadableBytes.ReadableBytesParseException;

/**
 * @author thomas
 * @date 25.06.2021
 *
 */
public class FlexiReadableBytesTest extends AWTest {
    public static void main(String[] args) throws FlexiMapperException, FlexiParserException, ParseException {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        try {
            ReadableBytes.parse("1Byte");
            throw new Exception("Expect a unknown unit exception");
        } catch (ReadableBytesParseException e) {
            // ok
        }
        assertThat(new ReadableBytes(2000000, false).format()).equals("2MB");
        assertThat(new ReadableBytes(2 * 1024 * 1024, true).format()).equals("2MiB");
        assertThat(new ReadableBytes(2 * 1024 * 1024).format()).equals("2MiB");
        assertThat(ReadableBytes.parse("1,5 MB 500kb").getBytes()).isNumber(2000 * 1000l);
        assertThat(ReadableBytes.parse("1,5MB").getBytes()).isNumber(1500 * 1000l);
        assertThat(ReadableBytes.parse("1,5MiB").getBytes()).isNumber(1.5 * 1024 * 1024);
        assertThat(ReadableBytes.parse("1MiB").format()).equals("1MiB");
        assertThat(ReadableBytes.parse("1MB").format()).equals("1MB");
        assertFalse(ReadableBytes.parse("1b").isKibi());
        assertTrue(ReadableBytes.parse("1 kib 1b").isKibi());
        assertTrue(FlexiUtils.jsonToObject("\"1B\"", ReadableBytes.TYPE).getBytes() == 1);
        // Restore from bytes
        assertTrue(FlexiUtils.jsonToObject("1024", ReadableBytes.TYPE).getBytes() == 1024);
        ReadableBytes bytes = ReadableBytes.fromBytes(System.currentTimeMillis());
        String readable = FlexiUtils.serializeMinimized(bytes);
        assertTrue(FlexiUtils.jsonToObject(readable, ReadableBytes.TYPE).getBytes() == bytes.getBytes());
    }
}
