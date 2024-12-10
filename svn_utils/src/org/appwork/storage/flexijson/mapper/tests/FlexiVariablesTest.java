/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.storage.flexijson.mapper.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.appwork.exceptions.WTFException;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiSerializer;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.ReferenceLoopException;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiVariableAccess;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiVariables;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.AWTest.AssertAnException.MODE;

/**
 * @author thomas
 * @date 21.11.2022
 *
 */
public class FlexiVariablesTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    // required to tell the mapper to scan the class for FlexiVariables annotations
    @FlexiVariables()
    public static class Container implements Storable {
        public static final SimpleTypeRef<FlexiVariablesTest.Container> TYPE = new SimpleTypeRef<FlexiVariablesTest.Container>(FlexiVariablesTest.Container.class);
        private Map<String, Object>                                     variables;

        public Map<String, Object> getVariables() {
            return variables;
        }

        public void setVariables(Map<String, Object> variables) {
            this.variables = variables;
        }

        @FlexiVariables(path = "i")
        private Map<String, Object> variables2;

        public Map<String, Object> getVariables2() {
            return variables;
        }

        public void setVariables2(Map<String, Object> variables) {
            this.variables = variables;
        }

        private int i;

        @FlexiVariableAccess("(variables2\\.i|i|e)")
        public int getI() {
            return i;
        }

        public void setI(int i) {
            this.i = i;
        }

        private int e;

        @FlexiVariableAccess("(variables\\..+|e|i)")
        public int getE() {
            return e;
        }

        public void setE(int i) {
            this.e = i;
        }

        private String str2;

        @FlexiVariableAccess("variables\\..+")
        public String getStr2() {
            return str2;
        }

        public void setStr2(String str2) {
            this.str2 = str2;
        }

        @FlexiVariableAccess("variables\\..+")
        private String strSameAs2;

        public String getStrSameAs2() {
            return strSameAs2;
        }

        public void setStrSameAs2(String strSameAs2) {
            this.strSameAs2 = strSameAs2;
        }

        private String str;
        private String str3;

        @FlexiVariableAccess("variables\\..+")
        public String getStr() {
            return str;
        }

        public void setStr(String str) {
            this.str = str;
        }

        @FlexiVariableAccess("variables\\..+")
        public String getStr3() {
            return str3;
        }

        public void setStr3(String str) {
            this.str3 = str;
        }

        @FlexiVariableAccess("intern")
        private SubCont intern2 = new SubCont();

        public SubCont getIntern2() {
            return intern2;
        }

        public void setIntern2(SubCont intern2) {
            this.intern2 = intern2;
        }

        private SubCont intern;

        public SubCont getIntern() {
            return intern;
        }

        public void setIntern(SubCont intern) {
            this.intern = intern;
        }

        private SubCont[] internArray;

        public SubCont[] getInternArray() {
            return internArray;
        }

        public void setInternArray(SubCont[] internArray) {
            this.internArray = internArray;
        }
    }

    public static class SubCont implements Storable {
        /**
         *
         */
        public SubCont() {
        }

        @FlexiVariableAccess("(.*)")
        private ArrayList<String> arrayList = new ArrayList<String>();

        public ArrayList<String> getArrayList() {
            return arrayList;
        }

        public void setArrayList(ArrayList<String> arrayList) {
            this.arrayList = arrayList;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        // TODO: Test FlexiInterface
        Deser.set(new FlexiSerializer() {
            /**
             * @see org.appwork.storage.flexijson.FlexiSerializer#getMapper(java.lang.Object[])
             */
            @Override
            protected FlexiJSonMapper getMapper(Object... context) {
                FlexiJSonMapper ret = super.getMapper(context);
                ret.setReferencesEnabled(true);
                return ret;
            }
        });
        Container c = new Container();
        HashMap<String, Object> vars = new HashMap<String, Object>();
        vars.put("a", "AAA");
        vars.put("i", 1);
        c.setVariables(vars);
        // $${escaped} ---> ${escaped}
        // ${$escaped} ---> ${escaped}
        // ${$$escaped} ---> ${$escaped}
        c.setStr("Ist $${dsad} ein A: ${variables[\"$}\"]} und eine Zahl: ${variables.i}  ${balbl$}aasd");
        // Ist ${#escaped} ein A: ${variables.a} und eine Zahl: ${variables.i}
        c.setStr2("${variables.a}");
        c.setStr3("${variable.doesnotexistandisNotpartoftheaccesslist}");
        c.setStrSameAs2("${variables.a}");
        SubCont intern = new SubCont();
        intern.setArrayList(new ArrayList<String>(Arrays.asList("a", "${variables.i}.${str}", "${intern.arrayList[0]}", "${[0]#p1}", "${arrayList[0]#p2}", "${[].arrayList[0]}", "${[][0]#p2}")));
        c.setIntern(intern);
        {
            SubCont a = new SubCont();
            a.setArrayList(new ArrayList<String>(Arrays.asList("a", "${[][0]#p2}")));
            SubCont b = new SubCont();
            b.setArrayList(new ArrayList<String>(Arrays.asList("b", "${internArray[0]arrayList[0]}", "${internArray[][][0]}", "${internArray[1].arrayList[0]}", "${internArray[].arrayList[0]}", "${arrayList[0]#p2}", "${[0]arrayList[0]#p3}")));
            SubCont[] internArray = new SubCont[] { a, b };
            c.setInternArray(internArray);
        }
        final FlexiJSonObject node = Deser.convert(c, FlexiJSonObject.TYPE);
        node.put("i", "${variables2.i}");
        node.put("e", "${variables.i}");
        node.put("intern2", "${intern}");
        {
            String json = Deser.get().toString(node, SC.READABLE);
            Container resultC = Deser.get().fromString(json, Container.TYPE);
            assertEquals(resultC.getInternArray()[0].arrayList.get(1), "a");
            assertEquals(resultC.getInternArray()[1].arrayList.get(1), "a");
            assertEquals(resultC.getInternArray()[1].arrayList.get(2), "b");
            // is actually the same instance!
            assertTrue(resultC.getInternArray()[1].arrayList.get(3) == resultC.getInternArray()[1].arrayList.get(4));
            assertEquals(resultC.getStr(), "Ist ${dsad} ein A: ${variables[\"$}\"]} und eine Zahl: " + resultC.getI() + "  ${balbl$}aasd");
            assertEquals(resultC.getStr2(), "AAA");
            assertEquals(resultC.getI(), 1);
            // actual same instance
            assertTrue(resultC.getStr2() == resultC.getStrSameAs2());
        }
        new AssertAnException<ReferenceLoopException>(MODE.CONTAINS) {
            @Override
            protected void run() throws ReferenceLoopException {
                // expected exception due to self reference
                try {
                    node.put("i", "${i}");
                } catch (FlexiMapperException e) {
                    throw new WTFException(e);
                }
                String json = Deser.get().toString(node, SC.READABLE);
                Container resultC = Deser.get().fromString(json, Container.TYPE);
            }
        };
        new AssertAnException<ReferenceLoopException>(MODE.CONTAINS) {
            @Override
            protected void run() throws ReferenceLoopException {
                // expected exception due to reference loop
                try {
                    node.put("i", "${e}");
                    node.put("e", "${i}");
                } catch (FlexiMapperException e) {
                    throw new WTFException(e);
                }
                String json = Deser.get().toString(node, SC.READABLE);
                Container resultC = Deser.get().fromString(json, Container.TYPE);
            }
        };
        new AssertAnException<NumberFormatException>(MODE.CONTAINS) {
            @Override
            protected void run() throws NumberFormatException {
                // this should throw a mapper exception, because variables.i is not in the access pattern, and thus the variable cannot be
                // resolved or mapped to an integer
                try {
                    node.put("i", "${variables.i}");
                    node.put("e", "${i}");
                } catch (FlexiMapperException e) {
                    throw new WTFException(e);
                }
                String json = Deser.get().toString(node, SC.READABLE);
                Container resultC = Deser.get().fromString(json, Container.TYPE);
            }
        };
    }
}
