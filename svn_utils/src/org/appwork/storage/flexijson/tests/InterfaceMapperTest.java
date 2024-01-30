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
package org.appwork.storage.flexijson.tests;

import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefault;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.testframework.AWTest;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 26.03.2021
 *
 */
public class InterfaceMapperTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    public static interface TestInterface2 {
        boolean getBool();
    }

    public static interface TestInterface {
        public int getInt();

        public void setInt(int i);

        @FlexiInterfaceDefault("3")
        public int getIntWithDef();

        @FlexiInterfaceDefault("[{}]")
        public TestInterface2[] getSubList();

        public TestInterface getSub();

        @FlexiInterfaceDefault("{bool:true}")
        public TestInterface2 getSubWithDefault();

        public void setSub(TestInterface sub);
    }

    public static class TestObj implements TestInterface {
        private int           i;
        private TestInterface sub;

        /**
         * @param j
         */
        public TestObj() {
            // TODO Auto-generated constructor stub
        }

        public TestObj(int i) {
            this.i = i;
        }

        public TestInterface getSub() {
            return sub;
        }

        public void setSub(TestInterface sub) {
            this.sub = sub;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.tests.InterfaceMapperTest.TestInterface#getInt()
         */
        @Override
        public int getInt() {            
            return i;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.tests.InterfaceMapperTest.TestInterface#getIntWithDef()
         */
        @Override
        public int getIntWithDef() {            
            return 9;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.tests.InterfaceMapperTest.TestInterface#setInt()
         */
        @Override
        public void setInt(int i) {
            this.i = i;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.tests.InterfaceMapperTest.TestInterface#getSubList()
         */
        @Override
        public TestInterface2[] getSubList() {            
            return null;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.tests.InterfaceMapperTest.TestInterface#getSubWithDefault()
         */
        @Override
        public TestInterface2 getSubWithDefault() {            
            return null;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        TestObj obj = new TestObj();
        obj.setSub(new TestObj(3));
        FlexiJSonMapper mapper = new FlexiJSonMapper() {
            protected InterfaceStorage createInterfaceInvocationHandler(CompiledType cType, FlexiJSonObject obj) throws SecurityException, NoSuchMethodException {
                return new InterfaceStorage(this, cType, obj);
            };
        };
        FlexiJSonNode node = mapper.objectToJsonNode(obj);
        TestInterface proxy = mapper.jsonToObject(node, new SimpleTypeRef<TestInterface>(TestInterface.class));
        TestInterface proxy2 = mapper.jsonToObject(FlexiUtils.copyOf(node), new SimpleTypeRef<TestInterface>(TestInterface.class));
        TestInterface proxy3 = mapper.jsonToObject(mapper.objectToJsonNode(obj), new SimpleTypeRef<TestInterface>(TestInterface.class));
        mapper.setIgnoreDefaultValuesEnabled(true);
        ((FlexiJSonObject) node).remove("sub");
        // may be != null in debug mode of the proxy.toString() method has been called by the IDE
        assertNull(proxy.getSub());
        System.out.println(proxy.toString());
        assertNotNull(proxy2.getSub());
        int oldInt = proxy2.getSub().getInt();
        proxy2.getSub().setInt(7);
        assertEquals(proxy2.getSub().getInt(), 7);
        proxy2.getSub().setInt(oldInt);
        assertEquals(proxy2.getSub().getInt(), oldInt);
        assertTrue(proxy.equals(proxy));
        assertFalse(proxy.equals(proxy2));
        assertTrue(proxy2.equals(proxy3));
        proxy2.setSub(null);
        assertTrue(proxy.equals(proxy2));
        assertTrue(proxy.equals(InterfaceStorage.shrink(proxy2)));
        assertFalse(proxy.equals(null));
        assertFalse(proxy.equals(obj));
        assertEquals(obj.getInt(), proxy.getInt());
        TestInterface old = proxy.getSub();
        proxy.setSub(obj);
        TestInterface newObj = proxy.getSub();
        // cache enabled
        assertTrue(newObj == obj);
        assertEquals(proxy.getIntWithDef(), 9);
        TestInterface defaultsProxy = mapper.jsonToObject(new FlexiJSONParser("{}").parse(), new SimpleTypeRef<TestInterface>(TestInterface.class));
        assertEquals(defaultsProxy.getIntWithDef(), 3);
        assertEquals(defaultsProxy.getSubWithDefault().getBool(), true);
    }
}
