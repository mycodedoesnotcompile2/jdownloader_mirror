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
package org.appwork.serializer.tests;

import java.io.ByteArrayInputStream;

import org.appwork.serializer.SC;
import org.appwork.storage.SimpleSerializer;
import org.appwork.storage.TypeRef;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.storage.flexijson.FlexiSerializer;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 25.09.2023
 *
 */
public class DeserTests extends AWTest {
    private SerializerInterface[] serializers;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        serializers = new SerializerInterface[] { new FlexiSerializer(), new SimpleSerializer() };
        try {
            test("{\"a\":\"line1\\r\\nline2\"}", "", TypeRef.HASHMAP, SC.SINGLE_LINE, SC.READABLE);
            throw new Exception("We expect an exception due to SINGLE_LINE and READABLE");
        } catch (org.appwork.storage.commonInterface.SerializerException e) {
            // expected
        }
        String result = test("{\"a\":\"line1\\r\\nline2\"}", "", TypeRef.HASHMAP, SC.SINGLE_LINE);
        assertTrue(result.split("[\r\n]+").length == 1);
        for (SerializerInterface c : serializers) {
            Object o = c.fromString("{\"a\":\"line1\\r\\nline2\",\"b\":\"line1\\r\\nline2\",\"c\":\"line1\\r\\nline2\",\"d\":\"line1\\r\\nline2\",\"e\":\"line1\\r\\nline2\"}", TypeRef.OBJECT);
            assertTrue(c.toString(o, SC.READABLE).split("[\r\n]+").length > 1);
        }
        // more to do -
    }

    /**
     * @param <T>
     * @param string
     * @param string2
     * @param singleLine
     * @return
     * @throws Exception
     */
    private <T> String test(String input, String expected, TypeRef<T> type, Object... context) throws Exception {
        T first = null;
        for (SerializerInterface s : serializers) {
            T res = s.fromString(input, type, context);
            if (first == null) {
                first = res;
            } else {
                assertEqualsDeep(first, res);
            }
        }
        first = null;
        for (SerializerInterface s : serializers) {
            T res = s.fromStream(new ByteArrayInputStream(input.getBytes("UTF-8")), type, context);
            if (first == null) {
                first = res;
            } else {
                assertEqualsDeep(first, res);
            }
        }
        first = null;
        for (SerializerInterface s : serializers) {
            T res = s.fromByteArray(input.getBytes("UTF-8"), type, context);
            if (first == null) {
                first = res;
            } else {
                assertEqualsDeep(first, res);
            }
        }
        String firstResult = null;
        for (SerializerInterface s : serializers) {
            String res = s.toString(first, context);
            if (firstResult == null) {
                firstResult = res;
            } else {
                assertEquals(firstResult, res);
            }
        }
        return firstResult;
    }

    public static void main(String[] args) {
        run();
    }
}
