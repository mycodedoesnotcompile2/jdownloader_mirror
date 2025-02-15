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

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.LEB128;

/**
 * @author daniel
 * @date Aug 24, 2023
 *
 */
@TestDependency({ "org.appwork.utils.LEB128" })
public class LEB128Test extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        for (Long mod : new long[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }) {
            for (Long testValue : new long[] { Long.MAX_VALUE, Long.MAX_VALUE - mod, mod + Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE - mod, mod + Short.MAX_VALUE, Short.MAX_VALUE, Short.MAX_VALUE - mod, mod + Byte.MAX_VALUE, Byte.MAX_VALUE, Byte.MAX_VALUE - 1, 1, 0 }) {
                bos.reset();
                LEB128.write(bos, testValue);
                final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
                assertEquals(testValue, LEB128.readLong(bis, true));
                if (testValue > Integer.MAX_VALUE) {
                    Exception e = null;
                    try {
                        bis.reset();
                        LEB128.readInt(bis, true);
                    } catch (ArithmeticException e2) {
                        e = e2;
                    }
                    assertNotNull(e);
                    assertEquals(ArithmeticException.class, e.getClass());
                }
                if (testValue <= Integer.MAX_VALUE) {
                    bis.reset();
                    assertEquals(testValue.intValue(), LEB128.readInt(bis, true));
                }
                if (testValue > Short.MAX_VALUE) {
                    Exception e = null;
                    try {
                        bis.reset();
                        LEB128.readShort(bis, true);
                    } catch (ArithmeticException e2) {
                        e = e2;
                    }
                    assertNotNull(e);
                    assertEquals(ArithmeticException.class, e.getClass());
                }
                if (testValue <= Short.MAX_VALUE) {
                    bis.reset();
                    assertEquals(testValue.shortValue(), LEB128.readShort(bis, true));
                }
                if (testValue > Byte.MAX_VALUE) {
                    Exception e = null;
                    try {
                        bis.reset();
                        LEB128.readByte(bis, true);
                    } catch (ArithmeticException e2) {
                        e = e2;
                    }
                    assertNotNull(e);
                    assertEquals(ArithmeticException.class, e.getClass());
                }
                if (testValue <= Byte.MAX_VALUE) {
                    bis.reset();
                    assertEquals(testValue.byteValue(), LEB128.readByte(bis, true));
                }
            }
        }
    }
}
