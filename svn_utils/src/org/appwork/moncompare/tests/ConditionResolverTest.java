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
package org.appwork.moncompare.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;

/**
 * @author Thomas
 * @date 06.05.2019
 *
 */
@TestDependency({ "org.appwork.moncompare.Conditions" })
public class ConditionResolverTest extends AWTest {
    /**
     *
     */
    /**
     *
     */
    public static class EmbededObject {
        public ArrayList<String> list = new ArrayList<String>();

        public ArrayList<String> getList() {
            return list;
        }

        public void setList(ArrayList<String> list) {
            this.list = list;
        }

        public ArrayList<String[]> getList2() {
            return list2;
        }

        public void setList2(ArrayList<String[]> list2) {
            this.list2 = list2;
        }

        public boolean isC() {
            return c;
        }

        public void setC(boolean c) {
            this.c = c;
        }

        public int getI() {
            return i;
        }

        public void setI(int i) {
            this.i = i;
        }

        {
            this.list.add("s1");
            this.list.add("s2");
        }
        public ArrayList<String[]> list2 = new ArrayList<String[]>();
        {
            this.list2.add(new String[] { "a", "b" });
            this.list2.add(new String[] { "a", "b" });
        }
        private boolean b = false;
        private boolean c = false;

        /**
         * @return the b
         */
        public boolean isB() {
            return this.b;
        }

        /**
         * @param b
         *            the b to set
         */
        public void setB(boolean b) {
            this.b = b;
        }

        private int i = 3;
    }

    /**
     * @param string
     * @param string2
     * @return
     */
    private static Object[] list(Object... strings) {
        return strings;
    }

    private static void eval(Object soll, Object ist) {
        if (!soll.toString().equals(String.valueOf(ist))) {
            throw new WTFException();
        }
    }

    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void runTest() throws Exception {
        Condition.THREAD_DEBUG.set(true);
        Condition.THREAD_LOGGER.set(LogV3.defaultLogger());
        HashMap<String, Object> regex = new HashMap<String, Object>();
        regex.put("a", "(.");
        regex.put("valid", "abc.def");
        regex.put("valid2", "abc\\ddef");
        regex.put("valid", "abc.def");
        regex.put("set", new ArrayList<String>(Arrays.asList("123", "643DFbg")));
        regex.put("list", new Object[] { true, 1, "string" });
        regex.put("listWithNull", new Object[] { true, 1, "string", null });
        {
            HashMap<String, Object> map2 = new HashMap<String, Object>();
            map2.put("a", true);
            map2.put("b", false);
            HashMap<String, Object> map3 = new HashMap<String, Object>();
            map3.put("a", true);
            map3.put("b", true);
            HashMap<String, Object> map = new HashMap<String, Object>();
            map.put("el1", map2);
            map.put("el2", map3);
            map.put("el3", map2);
            map.put("el4", map3);
            regex.put("map", map);
        }
        Object eval = Condition.resolve("a", regex);
        assertEquals(eval, regex.get("a"));
        eval = Condition.resolve("list[1]", regex);
        assertEquals(eval, 1);
        assertEquals(new Condition(Condition.$GET, "§a").evaluate(regex), "(.");
        assertEquals(new Condition("§concat", new Object[] { "§a" }).evaluate(regex), "(.");
        assertEquals(new Condition("§concat", new Object[] { "§a", " ", new Condition(Condition.$TO_UPPER_CASE, "§valid") }).evaluate(regex), "(. ABC.DEF");
        assertEquals(new Condition("§concat", new Object[] { "§a", " ", new Condition(Condition.$TO_LOWER_CASE, "§set[1]") }).evaluate(regex), "(. 643dfbg");
        assertEquals(new Condition(Condition.$SEARCH_AND_REPLACE, new Object[] { "§valid", "abc", "§a" }).evaluate(regex), "(..def");
        assertEquals(new Condition(Condition.$IF, new Object[] { new Condition("valid", new Condition("§eq", "abc.def")), "§a", "false" }).evaluate(regex), "(.");
        assertEquals(new Condition(Condition.$IF, new Object[] { new Condition("valid", new Condition("§eq", "abcdef")), "§a", "false" }).evaluate(regex), "false");

        assertEquals(new Condition(Condition.$SEARCH_AND_REPLACE, new Object[] { "§valid", "abc", "§a" }).evaluate(regex), "(..def");
        assertEquals(new Condition(Condition.$SEARCH_AND_REPLACE, new Object[] { "§valid", "a(.)c", "-$1", 1 }).evaluate(regex), "a-$1c.def");
        assertEquals(new Condition(Condition.$SEARCH_AND_REPLACE, new Object[] { "§valid", "a(.)c", "-$1", 1 }).option(Condition.OPTIONS_ALLOW_REFERENCES, true).evaluate(regex), "a-bc.def");
        assertEquals(new Condition(Condition.$SEARCH_AND_REPLACE, new Object[] { "§valid", "aBc", "-$1", 1 }).evaluate(regex), "abc.def");
    }
}
