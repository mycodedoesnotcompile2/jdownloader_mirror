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
package org.appwork.remoteapi.test;

import java.io.IOException;

import org.appwork.remoteapi.RemoteAPI;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;

/**
 * @author daniel
 * @date Apr 18, 2023
 *
 */
public class RemoteAPITest extends AWTest {

    public static enum TEST_ENUM {
        TEST1,
        TEST2;
    }

    public static void main(final String[] args) throws IOException {
        AWTest.run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        assertEquals(Boolean.TRUE, RemoteAPI.convert("true", Boolean.class));
        assertEquals(Boolean.TRUE, RemoteAPI.convert("\"true\"", Boolean.class));
        assertEquals(Boolean.TRUE, RemoteAPI.convert("true", Object.class));
        assertEquals("true", RemoteAPI.convert("\"true\"", Object.class));
        assertEquals(Boolean.FALSE, RemoteAPI.convert("false", Boolean.class));
        assertEquals(Boolean.FALSE, RemoteAPI.convert("\"false\"", Boolean.class));
        assertEquals(Boolean.FALSE, RemoteAPI.convert("false", Object.class));
        assertEquals("false", RemoteAPI.convert("\"false\"", Object.class));

        assertEquals(null, RemoteAPI.convert("null", Object.class));
        assertEquals("null", RemoteAPI.convert("\"null\"", Object.class));
        assertEquals(null, RemoteAPI.convert("null", String.class));
        assertEquals("null", RemoteAPI.convert("\"null\"", String.class));

        assertEquals("test", RemoteAPI.convert("test", Object.class));
        assertEquals("test", RemoteAPI.convert("\"test\"", Object.class));
        assertEquals("test", RemoteAPI.convert("test", String.class));
        assertEquals("test", RemoteAPI.convert("\"test\"", String.class));

        assertEquals("test\"", RemoteAPI.convert("test\"", Object.class));
        assertEquals("test\"", RemoteAPI.convert("test\"", String.class));

        assertEquals("{test", RemoteAPI.convert("{test", String.class));
        assertEquals("{test", RemoteAPI.convert("{test", Object.class));
        assertEquals("{test}", RemoteAPI.convert("{test}", String.class));
        assertEquals("{test}", RemoteAPI.convert("{test}", Object.class));
        assertEquals("test}", RemoteAPI.convert("test}", String.class));
        assertEquals("test}", RemoteAPI.convert("test}", Object.class));
        assertEquals("{test", RemoteAPI.convert("\"{test\"", String.class));
        assertEquals("{test", RemoteAPI.convert("\"{test\"", Object.class));
        assertEquals("{test}", RemoteAPI.convert("\"{test}\"", String.class));
        assertEquals("{test}", RemoteAPI.convert("\"{test}\"", Object.class));
        assertEquals("test}", RemoteAPI.convert("\"test}\"", String.class));
        assertEquals("test}", RemoteAPI.convert("\"test}\"", Object.class));

        assertEquals("[test", RemoteAPI.convert("[test", String.class));
        assertEquals("[test", RemoteAPI.convert("[test", Object.class));
        assertEquals("[test]", RemoteAPI.convert("[test]", String.class));
        assertEquals("[test]", RemoteAPI.convert("[test]", Object.class));
        assertEquals("test]", RemoteAPI.convert("test]", String.class));
        assertEquals("test]", RemoteAPI.convert("test]", Object.class));
        assertEquals("[test", RemoteAPI.convert("\"[test\"", String.class));
        assertEquals("[test", RemoteAPI.convert("\"[test\"", Object.class));
        assertEquals("[test]", RemoteAPI.convert("\"[test]\"", String.class));
        assertEquals("[test]", RemoteAPI.convert("\"[test]\"", Object.class));
        assertEquals("test]", RemoteAPI.convert("\"test]\"", String.class));
        assertEquals("test]", RemoteAPI.convert("\"test]\"", Object.class));

        assertEquals("\"test", RemoteAPI.convert("\"test", Object.class));
        assertEquals("\"test", RemoteAPI.convert("\"test", String.class));

        assertEquals(10, RemoteAPI.convert("10", Object.class));
        assertEquals("10", RemoteAPI.convert("\"10\"", Object.class));
        assertEquals(10, RemoteAPI.convert("10", Integer.class));
        assertEquals(10, RemoteAPI.convert("\"10\"", Integer.class));

        assertEquals(TEST_ENUM.TEST1, RemoteAPI.convert("TEST1", new TypeRef<TEST_ENUM>() {
        }.getType()));
        assertEquals(TEST_ENUM.TEST2, RemoteAPI.convert("TEST2", new TypeRef<TEST_ENUM>() {
        }.getType()));
        assertEquals(TEST_ENUM.TEST1, RemoteAPI.convert("\"TEST1\"", new TypeRef<TEST_ENUM>() {
        }.getType()));
        assertEquals(TEST_ENUM.TEST2, RemoteAPI.convert("\"TEST2\"", new TypeRef<TEST_ENUM>() {
        }.getType()));

        assertEquals(Long.MAX_VALUE, RemoteAPI.convert("" + Long.MAX_VALUE, Object.class));
        assertEquals(Long.MIN_VALUE, RemoteAPI.convert("" + Long.MIN_VALUE, Object.class));
        assertEquals(Long.MAX_VALUE, RemoteAPI.convert("" + Long.MAX_VALUE, Long.class));
        assertEquals(Long.MIN_VALUE, RemoteAPI.convert("" + Long.MIN_VALUE, Long.class));

        assertEquals(Integer.MAX_VALUE, RemoteAPI.convert("" + Integer.MAX_VALUE, Object.class));
        assertEquals(Integer.MIN_VALUE, RemoteAPI.convert("" + Integer.MIN_VALUE, Object.class));

        assertEquals(Integer.MAX_VALUE, RemoteAPI.convert("" + Integer.MAX_VALUE, Integer.class));
        assertEquals(Integer.MIN_VALUE, RemoteAPI.convert("" + Integer.MIN_VALUE, Integer.class));
    }

}
