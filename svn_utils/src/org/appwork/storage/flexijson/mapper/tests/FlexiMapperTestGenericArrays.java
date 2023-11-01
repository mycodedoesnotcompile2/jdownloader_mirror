/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher StraÃŸe 117
 *         90763 FÃ¼rth
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

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;

import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.test.TestClassWithGenericArrayType;
import org.appwork.storage.simplejson.mapper.test.TestClassWithGenericArrayType2;
import org.appwork.testframework.AWTest;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 25.06.2021
 *
 */
public class FlexiMapperTestGenericArrays extends AWTest {
    public static void main(String[] args) throws FlexiMapperException, FlexiParserException {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        testTestClassWithGenericArrayType();
        testTestClassWithGenericArrayType2();
    }

    public void testTestClassWithGenericArrayType2() throws NoSuchMethodException, Exception, FlexiMapperException, FlexiParserException {
        TestClassWithGenericArrayType2<String> inst = new TestClassWithGenericArrayType2<String>();
        FlexiJSonMapper mapper = new FlexiJSonMapper() {
            @Override
            protected boolean isAnnotationCommentsEnabled() {
                return true;
            };

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#isTypeCommentsEnabled()
             */
            @Override
            protected boolean isTypeCommentsEnabled() {
                return true;
            }
        };
        ClassCache cc = ClassCache.getClassCache(new TypeRef<TestClassWithGenericArrayType2<String>>() {
        }.getType());
        assertTrue(cc.getGetter("array").type instanceof GenericArrayType);
        inst.setArray(new String[] { "mystitng" });
        inst.setGenArray(new ArrayList[] { new ArrayList<String>(Arrays.asList("A", "AB")) });
        String jsonmini = FlexiUtils.serializeMinimized(inst);
        CompiledType ctype = CompiledType.create(new TypeRef<TestClassWithGenericArrayType2<String>>() {
        });
        Type genericArrayType = new TypeRef<ArrayList<String>[]>() {
        }.getType();
        FlexiJSonNode nodes = mapper.objectToJsonNode(inst, new TypeRef<TestClassWithGenericArrayType2<String>>() {
        });
        String json1 = new FlexiJSonPrettyStringify().toJSONString(nodes);
        // String jsonOld = JSonStorage.serializeToJson(inst);
        // TestClassWithGenericArrayType2<String> restoredOld = JSonStorage.restoreFromString(jsonOld, new
        // TypeRef<TestClassWithGenericArrayType2<String>>() {
        // });
        // assertEqualsDeep(inst, restoredOld);
        assertTrue(json1.contains("Type: Array<String>"));
        assertEquals(jsonmini, "{\"array\":[\"mystitng\"],\"genArray\":[[\"A\",\"AB\"]]}");
        TestClassWithGenericArrayType2<String> restored = FlexiUtils.jsonToObject(jsonmini, FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new TypeRef<TestClassWithGenericArrayType2<String>>() {
        });
        assertEqualsDeep(inst, restored);
    }

    public void testTestClassWithGenericArrayType() throws NoSuchMethodException, Exception, FlexiMapperException, FlexiParserException {
        ClassCache cc = ClassCache.getClassCache(new TypeRef<TestClassWithGenericArrayType>() {
        }.getType());
        assertTrue(cc.getGetter("genArray1").type instanceof GenericArrayType);
        TestClassWithGenericArrayType inst1 = new TestClassWithGenericArrayType();
        ArrayList<String>[] test = new ArrayList[] { new ArrayList<String>(Arrays.asList("entry")) };
        inst1.setGenArray1(test);
        String json1 = FlexiUtils.serializeConfigStorable(inst1);
        assertTrue(json1.contains("Type: Array<Array<String>>"));
        assertTrue(json1.contains("\"genArray1\":[[\"entry\"]]"));
        String jsonmini = FlexiUtils.serializeMinimized(inst1);
        assertEquals(jsonmini, "{\"genArray1\":[[\"entry\"]]}");
        TestClassWithGenericArrayType restored = FlexiUtils.jsonToObject(jsonmini, FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<TestClassWithGenericArrayType>(TestClassWithGenericArrayType.class));
        assertEqualsDeep(inst1, restored);
    }
}
