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
package org.appwork.storage.flexijson.mapper.tests;

import java.lang.reflect.Type;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.simplejson.mapper.test.GenList2DTestForSImpleJson.TEstENum;
import org.appwork.storage.simplejson.mapper.test.GenericListTypeTestClass;
import org.appwork.storage.simplejson.mapper.test.TestClass;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 25.06.2021
 *
 */
public class FlexiMapperTest extends AWTest {
    public static void main(String[] args) throws FlexiMapperException, FlexiParserException, ParseException {
        run();
    }

    protected void testOnTestClass() throws Exception {
        final Object obj = TestClass.createObject();
        compareTest(obj);
        compareTest(ClassWithSetsListMaps.filledInstance());
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        FlexiJSonValue intValue = (FlexiJSonValue) mapper.objectToJsonNode(1);
        assertEquals(intValue.getValue(), 1);
        FlexiJSonNode node = mapper.objectToJsonNode(obj);
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("objArray.[1].intArray[0]")).getValue(), 1);
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("objArray.[1][\"intArray\"][0]")).getValue(), 1);
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("genericClass.key")).getValue(), "mykey");
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath(".genericClass...key")).getValue(), "mykey");
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("genericClass[\"key\"]")).getValue(), "mykey");
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("genericClass.[\"key\"]")).getValue(), "mykey");
        assertEquals(((FlexiJSonValue) ((FlexiJSonObject) node).resolvePath("[genericClass].[\"key\"]")).getValue(), "mykey");
        String json = new FlexiJSonStringBuilder().toJSONString(mapper.objectToJsonNode(ClassWithSetsListMaps.filledInstance()));
        System.out.println(1);
    }

    protected <T> void compareTest(final T obj) throws Exception {
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        final FlexiJSonNode json = mapper.objectToJsonNode(null, obj, null);
        // System.out.println(new FlexiJSonPrettyStringify().toJSONString(json));
        // System.out.println(new PropertyJSonPrettyStringify().toJSONString(json));
        // final String jsonString = json.toString();
        final T re = (T) mapper.jsonToObject(json, new TypeRef<Object>() {
            public Type getType() {
                return obj.getClass();
            }
        });
        // System.out.println(JSonStorage.serializeToJson(obj));
        // prints true if mapperloop succeeded
        String a = JSonStorage.serializeToJson(obj);
        String b = JSonStorage.serializeToJson(re);
        assertEqualsDeep(a, b);
        assertEqualsDeep(obj, re);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        FlexiJSonNode result = new FlexiJSONParser("[\"string\"  , true]").setDebug(new StringBuilder()).parse();
        // Same test for simpleJSOn: org.appwork.storage.simplejson.mapper.test.GenList2DTestForSImpleJson
        {
            // primitives work, because simplemapper can "guess" them
            GenericListTypeTestClass<Integer> ist = new GenericListTypeTestClass<Integer>();
            ist.setList2D(new ArrayList<ArrayList<Integer>>(Arrays.asList(new ArrayList<Integer>(Arrays.asList(1, 2, 3)), new ArrayList<Integer>(Arrays.asList(1, 2, 3)))));
            String json = FlexiUtils.serializeToPrettyJson(ist);
            GenericListTypeTestClass<Integer> restored = FlexiUtils.jsonToObject(json, new TypeRef<GenericListTypeTestClass<Integer>>() {
            });
            assertEqualsDeep(ist, restored);
        }
        {
            GenericListTypeTestClass<Double> ist = new GenericListTypeTestClass<Double>();
            ist.setList2D(new ArrayList<ArrayList<Double>>(Arrays.asList(new ArrayList<Double>(Arrays.asList(1d, 2d, 3d)), new ArrayList<Double>(Arrays.asList(1d, 2d, 3d)))));
            SimpleMapper mapper = new SimpleMapper();
            String json = FlexiUtils.serializeToPrettyJson(ist);
            GenericListTypeTestClass<Double> restored = FlexiUtils.jsonToObject(json, new TypeRef<GenericListTypeTestClass<Double>>() {
            });
            assertEqualsDeep(ist, restored);
        }
        {
            GenericListTypeTestClass<TEstENum> ist = new GenericListTypeTestClass<TEstENum>();
            ist.setList2D(new ArrayList<ArrayList<TEstENum>>(Arrays.asList(new ArrayList<TEstENum>(Arrays.asList(TEstENum.A, TEstENum.B)), new ArrayList<TEstENum>(Arrays.asList(TEstENum.B, TEstENum.A)))));
            SimpleMapper mapper = new SimpleMapper();
            String json = FlexiUtils.serializeToPrettyJson(ist);
            GenericListTypeTestClass<TEstENum> restored = FlexiUtils.jsonToObject(json, new TypeRef<GenericListTypeTestClass<TEstENum>>() {
            });
            assertEqualsDeep(ist, restored);
        }
        testOnTestClass();
    }
}
