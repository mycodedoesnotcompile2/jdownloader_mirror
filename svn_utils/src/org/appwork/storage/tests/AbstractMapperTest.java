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
package org.appwork.storage.tests;

import java.math.BigInteger;
import java.util.Locale;

import org.appwork.storage.JSONMapper;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.simplejson.mapper.test.CharJSONTest;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 21.01.2022
 *
 */
public abstract class AbstractMapperTest extends AWTest {
    public void runWith(JSONMapper mapper) throws Exception {
        final Locale def = Locale.getDefault();
        try {
            String[] systems = { "arab", "arabext", "bali", "beng", "deva", "fullwide", "gujr", "guru", "hanidec", "khmr", "knda", "latn", "mlym", "mymr", "orya", "tamldec", "telu", "thai", "tibt" };
            for (final String system : systems) {
                final Locale loc = Locale.forLanguageTag("en-US-u-nu-" + system);
                Locale.setDefault(loc);
                floatNumberFormatTests(mapper);
                fixedNumberFormatTests(mapper);
                undefinedTests(mapper);
                assertEquals("10", mapper.stringToObject("10", String.class));
                assertEquals(10, mapper.stringToObject("10", int.class));
                assertEquals("1.0", mapper.stringToObject("1.0", String.class));
                assertEquals("false", mapper.stringToObject("false", String.class));
                assertEquals(0.0d, mapper.stringToObject("null", double.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Double>(Double.class)));
                assertEquals(0.0f, mapper.stringToObject("null", float.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Float>(Float.class)));
                assertEquals((long) 0, mapper.stringToObject("null", long.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Long>(Long.class)));
                assertEquals(0, mapper.stringToObject("null", int.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Integer>(Integer.class)));
                assertEquals((char) 0, mapper.stringToObject("0", char.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Character>(Character.class)));
                assertEquals((short) 0, mapper.stringToObject("null", short.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Short>(Short.class)));
                assertEquals((byte) 0, mapper.stringToObject("null", byte.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Byte>(Byte.class)));
                assertEquals(false, mapper.stringToObject("null", boolean.class));
                assertEquals(null, mapper.stringToObject("null", new SimpleTypeRef<Boolean>(Boolean.class)));
                SimpleStorable obj = new SimpleStorable();
                obj.setB(true);
                obj.setBa(new boolean[] { true, false, true });
                obj.setI(12367);
                obj.setL(System.currentTimeMillis());
                obj.setLa(new long[] { System.currentTimeMillis(), 0, System.currentTimeMillis() });
                obj.setIa(new int[] { 1, 2, 3, 4, 5 });
                obj.setString("Hello again");
                obj.setStringa(new String[] { "a", "b", "C" });
                String json = mapper.objectToString(obj);
                // HashMap<String, Object> map = mapper.stringToObject(json, TypeRef.HASHMAP);
                SimpleStorable restored = mapper.stringToObject(json, new SimpleTypeRef<SimpleStorable>(SimpleStorable.class));
                assertEqualsDeep(restored, obj);
                new CharJSONTest().runTest(mapper);
            }
        } finally {
            Locale.setDefault(def);
        }
    }

    public void undefinedTests(final JSONMapper mapper) throws Exception {
        assertEquals(null, mapper.stringToObject("undefined", String.class));
        assertEquals(null, mapper.stringToObject(" undefined", String.class));
        assertEquals(null, mapper.stringToObject(" undefined ", String.class));
        assertEquals(null, mapper.stringToObject("undefined ", String.class));
    }

    public void fixedNumberFormatTests(final JSONMapper mapper) throws Exception {
        for (int radix : new int[] { 2, 8, 10, 16 }) {
            for (int num = Short.MIN_VALUE; num < Short.MAX_VALUE; num += 20) {
                final boolean isNegativ = num < 0;
                final String numberString = Integer.toString(Math.abs(num), radix);
                switch (radix) {
                case 2:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0b" + numberString, Integer.class));
                    } else {
                        assertEquals(num, mapper.stringToObject("0b" + numberString, Integer.class));
                        assertEquals(num, mapper.stringToObject("+0b" + numberString, Integer.class));
                    }
                    break;
                case 8:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0o" + numberString, Integer.class));
                        setImplicitOctalSupported(true);
                        assertEquals(num, mapper.stringToObject("-0" + numberString, Integer.class));
                        setImplicitOctalSupported(false);
                        assertEquals(Integer.parseInt("-" + numberString), mapper.stringToObject("-0" + numberString, Integer.class));
                    } else {
                        assertEquals(num, mapper.stringToObject("0o" + numberString, Integer.class));
                        assertEquals(num, mapper.stringToObject("+0o" + numberString, Integer.class));
                        setImplicitOctalSupported(true);// implicit octal allowed
                        if (isImplicitOctalSupported()) {
                            assertEquals(num, mapper.stringToObject("0" + numberString, Integer.class));
                            assertEquals(num, mapper.stringToObject("+0" + numberString, Integer.class));
                        }
                        setImplicitOctalSupported(false);// implicit octal not allowed
                        if (!isImplicitOctalSupported()) {
                            assertEquals(Integer.parseInt(numberString), mapper.stringToObject("0" + numberString, Integer.class));
                            assertEquals(Integer.parseInt(numberString), mapper.stringToObject("+0" + numberString, Integer.class));
                        }
                    }
                    break;
                case 10:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-" + numberString, Integer.class));
                    } else {
                        assertEquals(num, mapper.stringToObject("+" + numberString, Integer.class));
                    }
                    break;
                case 16:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0x" + numberString, Integer.class));
                    } else {
                        assertEquals(num, mapper.stringToObject("0x" + numberString, Integer.class));
                        assertEquals(num, mapper.stringToObject("+0x" + numberString, Integer.class));
                    }
                    break;
                default:
                    throw new Exception("unsupported:" + radix);
                }
            }
        }
        setBigIntegerSupported(true);
        if (isBigIntegerSupported()) {
            final Class<? extends Number> numClass = Number.class;
            BigInteger num = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE);
            for (int radix : new int[] { 2, 8, 10, 16 }) {
                final boolean isNegativ = BigInteger.ZERO.compareTo(num) > 0;
                final String numberString = num.abs().toString(radix);
                switch (radix) {
                case 2:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0b" + numberString, numClass));
                    } else {
                        assertEquals(num, mapper.stringToObject("0b" + numberString, numClass));
                        assertEquals(num, mapper.stringToObject("+0b" + numberString, numClass));
                    }
                    break;
                case 8:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0o" + numberString, numClass));
                        setImplicitOctalSupported(true);
                        assertEquals(num, mapper.stringToObject("-0" + numberString, numClass));
                        setImplicitOctalSupported(false);
                        assertEquals(new BigInteger("-" + numberString), mapper.stringToObject("-0" + numberString, numClass));
                    } else {
                        assertEquals(num, mapper.stringToObject("0o" + numberString, numClass));
                        assertEquals(num, mapper.stringToObject("+0o" + numberString, numClass));
                        setImplicitOctalSupported(true);// implicit octal allowed
                        if (isImplicitOctalSupported()) {
                            assertEquals(num, mapper.stringToObject("0" + numberString, numClass));
                            assertEquals(num, mapper.stringToObject("+0" + numberString, numClass));
                        }
                        setImplicitOctalSupported(false);// implicit octal not allowed
                        if (!isImplicitOctalSupported()) {
                            assertEquals(new BigInteger(numberString), mapper.stringToObject("0" + numberString, numClass));
                            assertEquals(new BigInteger(numberString), mapper.stringToObject("+0" + numberString, numClass));
                        }
                    }
                    break;
                case 10:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-" + numberString, numClass));
                    } else {
                        assertEquals(num, mapper.stringToObject("+" + numberString, numClass));
                    }
                    break;
                case 16:
                    if (isNegativ) {
                        assertEquals(num, mapper.stringToObject("-0x" + numberString, numClass));
                    } else {
                        assertEquals(num, mapper.stringToObject("0x" + numberString, numClass));
                        assertEquals(num, mapper.stringToObject("+0x" + numberString, numClass));
                    }
                    break;
                default:
                    throw new Exception("unsupported:" + radix);
                }
            }
        }
        setBigIntegerSupported(false);
        if (!isBigIntegerSupported()) {
            final Class<? extends Number> numClass = Number.class;
            final BigInteger num = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(System.currentTimeMillis()));
            new AssertAnException<NumberFormatException>(AssertAnException.MODE.CONTAINS) {
                @Override
                protected void run() throws Exception {
                    mapper.stringToObject(num.toString(), numClass);
                }
            };
        }
    }

    protected boolean NaNSupported           = false;
    protected boolean ImplicitOctalSupported = false;
    protected boolean BigIntegerSupported    = false;

    public void setBigIntegerSupported(boolean bigIntegerSupported) {
        BigIntegerSupported = bigIntegerSupported;
    }

    public boolean isImplicitOctalSupported() {
        return ImplicitOctalSupported;
    }

    public void setImplicitOctalSupported(boolean implicitOctalSupported) {
        ImplicitOctalSupported = implicitOctalSupported;
    }

    public void setNaNSupported(boolean naNSupported) {
        NaNSupported = naNSupported;
    }

    protected boolean isNaNSupported() {
        return NaNSupported;
    }

    protected boolean isBigIntegerSupported() {
        return BigIntegerSupported;
    }

    public void floatNumberFormatTests(JSONMapper mapper) throws Exception {
        setNaNSupported(true);
        if (isNaNSupported()) {
            assertEquals(Float.NaN, mapper.stringToObject("NaN", Float.class));
            assertEquals(Double.NaN, mapper.stringToObject("NaN", Double.class));
        }
        for (int n = -1; n < 2; n++) {
            for (int index = 1; index < 23; index++) {
                double num = Math.pow(10, index);
                if (num < 0) {
                    break;
                }
                num = num * n;
                String numberString = Double.toString(num);
                Double val = mapper.stringToObject(numberString, Double.class).doubleValue();
                Double diff = num - val;
                assertTrue(diff < 0.01f);
                numberString = n + "e" + index; // float without dot but e
                diff = num - mapper.stringToObject(numberString, Double.class).doubleValue();
                assertTrue(diff < 0.01f);
                numberString = n + "e+" + index; // float without dot but e+
                diff = num - mapper.stringToObject(numberString, Double.class).doubleValue();
                assertTrue(diff < 0.01f);
            }
        }
        for (float num = Short.MIN_VALUE; num < Short.MAX_VALUE; num += 0.33f) {
            final boolean isNegativ = num < 0;
            for (String numberString : new String[] { Float.toString(Math.abs(num)), String.format(Locale.ROOT, "%e", Math.abs(num)), String.format(Locale.ROOT, "%E", Math.abs(num)) }) {
                Float diff = num - mapper.stringToObject((isNegativ ? "-" : "") + numberString, Float.class).floatValue();
                assertTrue(diff < 0.01f);
                if (!isNegativ) {
                    diff = num - mapper.stringToObject((isNegativ ? "-" : "+") + numberString, Float.class).floatValue();
                    assertTrue(diff < 0.01f);
                }
            }
        }
        for (double num = Short.MIN_VALUE; num < Short.MAX_VALUE; num += 0.33d) {
            final boolean isNegativ = num < 0;
            for (String numberString : new String[] { Double.toString(Math.abs(num)), String.format(Locale.ROOT, "%e", Math.abs(num)), String.format(Locale.ROOT, "%E", Math.abs(num)) }) {
                Double diff = num - mapper.stringToObject((isNegativ ? "-" : "") + numberString, Double.class).doubleValue();
                assertTrue(diff < 0.01d);
                if (!isNegativ) {
                    diff = num - mapper.stringToObject((isNegativ ? "-" : "+") + numberString, Double.class).doubleValue();
                    assertTrue(diff < 0.01d);
                }
            }
        }
    }
}
