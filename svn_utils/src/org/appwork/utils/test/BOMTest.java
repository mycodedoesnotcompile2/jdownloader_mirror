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
package org.appwork.utils.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.IO;

/**
 * @author daniel
 * @date Feb 9, 2021
 *
 */
public class BOMTest extends AWTest {
    public static void main(final String[] args) throws FlexiParserException, UnsupportedEncodingException, InterruptedException {
        run();
    }

    private static byte[] toBOMByteArray(final String string, IO.BOM bom) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        bos.write(bom.getBOM());
        bos.write(string.getBytes(bom.getCharSet()));
        return bos.toByteArray();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        final String normal = "Dies ist ein Test ÖÄÜ 😊 统一码";
        for (IO.BOM bom : IO.BOM.values()) {
            final String check = IO.readInputStreamToString(new ByteArrayInputStream(toBOMByteArray(normal, bom)));
            if (!normal.equals(check)) {
                throw new Exception("failed:" + bom + "|" + check);
            }
        }
        for (String charSetString : new String[] { "UTF-8", "UTF-16", "x-UTF-16LE-BOM", "X-UTF-32BE-BOM", "X-UTF-32LE-BOM" }) {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final Charset charSet = Charset.forName(charSetString);
            bos.write(normal.getBytes(charSet));
            final String check = IO.readInputStreamToString(new ByteArrayInputStream(bos.toByteArray()));
            if (!normal.equals(check)) {
                throw new Exception("failed:" + charSet + "|" + check);
            }
        }
        assertEqualsDeep(normal.getBytes("UTF-16"), toBOMByteArray(normal, IO.BOM.UTF16BE));
        assertEqualsDeep(normal.getBytes("x-UTF-16LE-BOM"), toBOMByteArray(normal, IO.BOM.UTF16LE));
        assertEqualsDeep(normal.getBytes("X-UTF-32BE-BOM"), toBOMByteArray(normal, IO.BOM.UTF32BE));
        assertEqualsDeep(normal.getBytes("X-UTF-32LE-BOM"), toBOMByteArray(normal, IO.BOM.UTF32LE));
        for (final String test : new String[] { "", "1", "12", "123", "1234" }) {
            final String check = IO.readInputStreamToString(new ByteArrayInputStream(test.getBytes("UTF-8")));
            if (!test.equals(check)) {
                throw new Exception("failed no bom!");
            }
        }
    }
}
