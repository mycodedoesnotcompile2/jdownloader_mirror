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
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.parserextension.TimeSpanParserExtension;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.testframework.AWTest;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 22.10.2022
 *
 */
public class FlexiBasicFieldTypesTest extends AWTest {
    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        FlexiJSONParser parser = new FlexiJSONParser("{\"timespan\":1Y!365}");
        parser.addExtension(new TimeSpanParserExtension());
        ObjectContainsAll restored = (ObjectContainsAll) mapper.jsonToObject(parser.parse(), CompiledType.create(ObjectContainsAll.class));
        assertThat(restored.getTimespan()).is(TimeSpan.parse("1Y!365"));
        ObjectContainsAll ie = new ObjectContainsAll();
        final FlexiJSonNode node = mapper.objectToJsonNode(ie);
        String serialized = new FlexiJSonStringBuilder().toJSONString(node);
        String expected = "{\"arrayList\":[\"eins\",\"drei\"],\"booleanWrapper\":true,\"booleanWrapperNull\":null,\"bytePrimitive\":127,\"byteWrapper\":-128,\"byteWrapperNull\":null,\"charPrimitive\":\"\uffff\",\"character\":\"\u25cb\",\"characterNull\":null,\"charsequence\":\"char seq\",\"charsequenceNull\":null,\"date\":\"2022-01-02T01:02:03.123+01:00\",\"doublePrimitive\":1.7976931348623157E308,\"doubleWrapper\":4.9E-324,\"doubleWrapperNull\":null,\"enumField\":\"DIR\",\"enumFieldNull\":null,\"floatPrimitive\":3.4028235E38,\"floatWrapper\":1.4E-45,\"floatWrapperNull\":null,\"hashMap\":{\"eins\":1,\"zwei\":2},\"hashSet\":[1,4,7],\"intArray\":[1,2,3],\"intPrimitive\":2147483647,\"intWrapper\":-2147483648,\"intWrapperNull\":null,\"integerArray\":[1,null,3],\"interfaceField\":{\"bool\":true},\"list\":[\"eins\",\"drei\"],\"listWithoutGen\":[\"eins\",\"drei\"],\"longPrimitive\":-9223372036854775808,\"longWrapper\":9223372036854775807,\"longWrapperNull\":null,\"map\":{\"eins\":1,\"zwei\":2},\"mapWithoutGen\":{\"eins\":1,\"zwei\":2},\"nullList\":null,\"nullMap\":null,\"nullSet\":null,\"objectHashSet\":[1,4,7],\"objectWithByte\":127,\"objectWithFloat\":1.4E-45,\"set\":[1,4,7],\"setWithoutGen\":[1,4,7],\"shortNull\":null,\"shortPrimitive\":32767,\"shortWrapper\":-32768,\"stringField\":\"fsdajbfdshbdfs98h\",\"stringFieldNull\":null,\"timespan\":\"10h15m\",\"booleanPrimitive\":false}";
        // System.out.println(serialized);
        // System.err.println(expected);
        assertEquals(serialized, expected);
        FlexiJSonNode restoredNode = new FlexiJSONParser(serialized).parse();
        assertEquals(serialized, new FlexiJSonStringBuilder().toJSONString(restoredNode));
        ObjectContainsAll restoredIE = mapper.jsonToObject(restoredNode, new SimpleTypeRef<ObjectContainsAll>(ObjectContainsAll.class));
        System.out.println(FlexiUtils.serializeToPrettyJson(restoredIE));
        System.out.println(FlexiUtils.serializeToPrettyJson(ie));
        // these objects should not equal, because we have a hashset assigned to an object field in ie. After restoring, this hashset will
        // become a linkedlist (object auto Mapping)
        assertEqualsDeepNot(restoredIE, ie);
        final FlexiJSonNode node2 = mapper.objectToJsonNode(restoredIE);
        String serialized2 = new FlexiJSonStringBuilder().toJSONString(node2);
        // however the json representation must be the same
        assertEquals(serialized, serialized2);
    }

    public static void main(String[] args) {
        run();
    }
}
