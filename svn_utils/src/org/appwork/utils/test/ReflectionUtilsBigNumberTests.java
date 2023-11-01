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
package org.appwork.utils.test;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.appwork.testframework.AWTest;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author daniel
 * @date Aug 17, 2023
 *
 */
public class ReflectionUtilsBigNumberTests extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        Number bigDecimalFloat = new BigDecimal("1.01") {
            public String toString() {
                return super.toString();
            };
        };
        Number bigDecimalFixed = new BigDecimal("10") {
            public String toString() {
                return super.toString();
            };
        };
        Number bigInteger = new BigInteger("1") {
            public String toString() {
                return super.toString();
            };
        };
        // bigDecimalFloat
        assertTrue(ReflectionUtils.hasDecimalPlaces(bigDecimalFloat));
        assertTrue(Clazz.isFloatingPointNumber(bigDecimalFloat.getClass()));
        assertFalse(Clazz.isFixedPointNumber(bigDecimalFloat.getClass()));
        // bigDecimalFixed
        assertFalse(ReflectionUtils.hasDecimalPlaces(bigDecimalFixed));
        assertTrue(Clazz.isFloatingPointNumber(bigDecimalFixed.getClass()));
        assertFalse(Clazz.isFixedPointNumber(bigDecimalFixed.getClass()));
        // bigInteger
        assertFalse(ReflectionUtils.hasDecimalPlaces(bigInteger));
        assertFalse(Clazz.isFloatingPointNumber(bigInteger.getClass()));
        assertTrue(Clazz.isFixedPointNumber(bigInteger.getClass()));
        //
        bigDecimalFloat = new BigDecimal(Long.MAX_VALUE + "");
        bigInteger = new BigInteger(Long.MAX_VALUE + "");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertFalse(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isIntRange(bigInteger));
        assertFalse(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isShortRange(bigInteger));
        assertFalse(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Integer.MAX_VALUE + "");
        bigInteger = new BigInteger(Integer.MAX_VALUE + "");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertTrue(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isIntRange(bigInteger));
        assertFalse(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isShortRange(bigInteger));
        assertFalse(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Short.MAX_VALUE + "");
        bigInteger = new BigInteger(Short.MAX_VALUE + "");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertTrue(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isIntRange(bigInteger));
        assertTrue(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isShortRange(bigInteger));
        assertTrue(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Byte.MAX_VALUE + "");
        bigInteger = new BigInteger(Byte.MAX_VALUE + "");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertTrue(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isIntRange(bigInteger));
        assertTrue(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isShortRange(bigInteger));
        assertTrue(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Long.MAX_VALUE + "1");
        bigInteger = new BigInteger(Long.MAX_VALUE + "1");
        assertNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNull(ReflectionUtils.getLongValue(bigInteger));
        assertFalse(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isLongRange(bigInteger));
        assertFalse(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isIntRange(bigInteger));
        assertFalse(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isShortRange(bigInteger));
        assertFalse(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Integer.MAX_VALUE + "1");
        bigInteger = new BigInteger(Integer.MAX_VALUE + "1");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertFalse(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isIntRange(bigInteger));
        assertFalse(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isShortRange(bigInteger));
        assertFalse(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Short.MAX_VALUE + "1");
        bigInteger = new BigInteger(Short.MAX_VALUE + "1");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getFloatValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getDoubleValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertNotNull(ReflectionUtils.getFloatValue(bigInteger));
        assertNotNull(ReflectionUtils.getDoubleValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertTrue(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isIntRange(bigInteger));
        assertFalse(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isShortRange(bigInteger));
        assertFalse(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = new BigDecimal(Byte.MAX_VALUE + "1");
        bigInteger = new BigInteger(Byte.MAX_VALUE + "1");
        assertNotNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getFloatValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getDoubleValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getLongValue(bigInteger));
        assertNotNull(ReflectionUtils.getFloatValue(bigInteger));
        assertNotNull(ReflectionUtils.getDoubleValue(bigInteger));
        assertTrue(ReflectionUtils.isLongRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isLongRange(bigInteger));
        assertTrue(ReflectionUtils.isIntRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isIntRange(bigInteger));
        assertTrue(ReflectionUtils.isShortRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isShortRange(bigInteger));
        assertTrue(ReflectionUtils.isCharRange(bigDecimalFloat));
        assertTrue(ReflectionUtils.isCharRange(bigInteger));
        assertFalse(ReflectionUtils.isByteRange(bigDecimalFloat));
        assertFalse(ReflectionUtils.isByteRange(bigInteger));
        //
        bigDecimalFloat = BigDecimal.valueOf(Float.MAX_VALUE);
        assertNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getFloatValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getDoubleValue(bigDecimalFloat));
        //
        bigDecimalFloat = BigDecimal.valueOf(Float.MAX_VALUE).multiply(BigDecimal.TEN);
        assertNull(ReflectionUtils.getLongValue(bigDecimalFloat));
        assertNull(ReflectionUtils.getFloatValue(bigDecimalFloat));
        assertNotNull(ReflectionUtils.getDoubleValue(bigDecimalFloat));
        //
        BigDecimal bigDecimalDouble = BigDecimal.valueOf(Double.MAX_VALUE);
        assertNull(ReflectionUtils.getLongValue(bigDecimalDouble));
        assertNull(ReflectionUtils.getFloatValue(bigDecimalDouble));
        assertNotNull(ReflectionUtils.getDoubleValue(bigDecimalDouble));
        //
        bigDecimalDouble = BigDecimal.valueOf(Double.MAX_VALUE).add(BigDecimal.ONE);
        assertNull(ReflectionUtils.getLongValue(bigDecimalDouble));
        assertNull(ReflectionUtils.getFloatValue(bigDecimalDouble));
        assertNull(ReflectionUtils.getDoubleValue(bigDecimalDouble));
        //
        bigDecimalDouble = BigDecimal.valueOf(Double.MAX_VALUE).subtract(BigDecimal.ONE);
        assertNull(ReflectionUtils.getLongValue(bigDecimalDouble));
        assertNull(ReflectionUtils.getFloatValue(bigDecimalDouble));
        assertNull(ReflectionUtils.getDoubleValue(bigDecimalDouble));
    }
}
