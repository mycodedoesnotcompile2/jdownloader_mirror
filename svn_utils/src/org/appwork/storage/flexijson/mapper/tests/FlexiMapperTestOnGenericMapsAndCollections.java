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
package org.appwork.storage.flexijson.mapper.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.reflection.tests.GenericsInDifferentLayersFullyResolved;
import org.appwork.utils.reflection.tests.ReReGenericDefinition;

/**
 * @author thomas
 * @date 25.06.2021
 *
 */
public class FlexiMapperTestOnGenericMapsAndCollections extends AWTest {
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
        testOnGenericsInDifferentLayersFullyResolved();
        testOnReReGenericDefinition();
        HashMap<String, HashMap<String, ArrayList<Boolean>>> obj = new HashMap<String, HashMap<String, ArrayList<Boolean>>>();
        ArrayList<Boolean> list = new ArrayList<Boolean>(Arrays.asList(true, false));
        HashMap<String, ArrayList<Boolean>> innerMap = new HashMap<String, ArrayList<Boolean>>();
        innerMap.put("InnerKey", list);
        obj.put("outerKey2", innerMap);
        String serialized = FlexiUtils.serializeMinimized(obj);
        assertEquals(serialized, "{\"outerKey2\":{\"InnerKey\":[true,false]}}");
        HashMap<String, HashMap<String, ArrayList<Boolean>>> restored = FlexiUtils.jsonToObject(serialized, new TypeRef<HashMap<String, HashMap<String, ArrayList<Boolean>>>>() {
        });
        assertEqualsDeep(restored, obj);
    }

    public void testOnGenericsInDifferentLayersFullyResolved() throws FlexiMapperException, Exception, FlexiParserException {
        GenericsInDifferentLayersFullyResolved obj = new GenericsInDifferentLayersFullyResolved();
        obj.put("mykey", 123l);
        String serialized = FlexiUtils.serializeToPrettyJson(obj);
        assertEquals(serialized, "{\"mykey\":123}");
        GenericsInDifferentLayersFullyResolved restored = FlexiUtils.jsonToObject(serialized, new SimpleTypeRef<GenericsInDifferentLayersFullyResolved>(GenericsInDifferentLayersFullyResolved.class));
        assertEqualsDeep(restored, obj);
    }

    public void testOnReReGenericDefinition() throws FlexiMapperException, Exception, FlexiParserException {
        ReReGenericDefinition testobj = new ReReGenericDefinition();
        testobj.put("Test", "Value");
        String serialized = FlexiUtils.serializeToPrettyJson(testobj);
        assertEquals(serialized, "{\"Test\":\"Value\"}");
        ReReGenericDefinition restored = FlexiUtils.jsonToObject(serialized, new SimpleTypeRef<ReReGenericDefinition>(ReReGenericDefinition.class));
        assertEqualsDeep(restored, testobj);
    }
}
