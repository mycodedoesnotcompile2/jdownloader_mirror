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
package org.appwork.utils.tests;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.utils.ReflectionUtils;

/**
 * @author thomas
 * @date 03.12.2021
 *
 */
public class ReflectionUtilsTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    public static enum TestEnum {
        A,
        B() {
        };
    }

    @Override
    public void runTest() throws Exception {
        {
            Object[] retA = ReflectionUtils.getEnumValues(TestEnum.A.getClass());
            Object[] retB = ReflectionUtils.getEnumValues(TestEnum.B.getClass());
            assertEqualsDeep(retA, retB);
            assertNotNull(retA);
        }
        {
            Object retA = ReflectionUtils.getEnumValueOf(TestEnum.A.getClass(), "A");
            Object retB = ReflectionUtils.getEnumValueOf(TestEnum.B.getClass(), "A");
            assertEqualsDeep(retA, retB);
            assertNotNull(retA);
        }
        if ((ReflectionUtils.getComponentClass(new TypeRef<Set<String>>() {
        }.getType()) != String.class)) {
            throw new Exception("getComponentClass Test Failed");
        }
        if (ReflectionUtils.getComponentClass(new TypeRef<List<Boolean>>() {
        }.getType()) != Boolean.class) {
            throw new Exception("getComponentClass Test Failed");
        }
        if (ReflectionUtils.getComponentClass(new TypeRef<Map<String, Integer>>() {
        }.getType()) != Integer.class) {
            throw new Exception("getComponentClass Test Failed");
        }
        if (ReflectionUtils.getComponentClass(new TypeRef<String>() {
        }.getType()) != null) {
            throw new Exception("getComponentClass Test Failed");
        }
        assertFalse(ReflectionUtils.isListOrArray(Collection.class));
        assertFalse(ReflectionUtils.isListOrArray(HashSet.class));
        // NOTE THAT (double)3.4028235E38 != (float)3.4028235E38
        assertTrue(ReflectionUtils.isFloatRange(3.4028234E38));
        assertTrue(ReflectionUtils.isFloatRange(Float.MIN_VALUE));
        assertTrue(ReflectionUtils.isFloatRange(Float.MAX_VALUE));
        assertTrue(ReflectionUtils.isFloatRange(-Float.MAX_VALUE));
        assertTrue(ReflectionUtils.isFloatRange(3.4028235E38));
        assertFalse(ReflectionUtils.isFloatRange(3.4028236E38));
        assertTrue(ReflectionUtils.isDoubleRange(Double.MAX_VALUE));
        assertTrue(ReflectionUtils.isDoubleRange(Double.MAX_VALUE));
        assertTrue(ReflectionUtils.isDoubleRange(-Double.MAX_VALUE));
    }
}
