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

import java.util.ArrayList;

import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.FlexiVisitor;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 27.07.2023
 *
 */
public class FlexiUtilsWalkTest extends AWTest {
    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        FlexiJSonMapper mapper = new FlexiJSonMapper() {
            /*
             * (non-Javadoc)
             * 
             * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#isAnnotationCommentsEnabled()
             */
            @Override
            protected boolean isAnnotationCommentsEnabled() {
                return true;
            }
        };
        ArrayList<String> expect = FlexiUtils
                .jsonToObject(
                        "[\"Open .\",\"Open .arrayList\",\"Value .arrayList[0] = eins\",\"Value .arrayList[1] = drei\",\"Close .arrayList\",\"Value .booleanWrapper = true\",\"Value .booleanWrapperNull = null\",\"Value .bytePrimitive = 127\",\"Value .byteWrapper = -128\",\"Value .byteWrapperNull = null\",\"Value .charPrimitive = \uffff\",\"Value .character = \u25cb\",\"Value .characterNull = null\",\"Value .charsequence = char seq\",\"Value .charsequenceNull = null\",\"Value .date = 2022-01-02T01:02:03.123+01:00\",\"Value .doublePrimitive = 1.7976931348623157E308\",\"Value .doubleWrapper = 4.9E-324\",\"Value .doubleWrapperNull = null\",\"Value .enumField = DIR\",\"Value .enumFieldNull = null\",\"Value .floatPrimitive = 3.4028235E38\",\"Value .floatWrapper = 1.4E-45\",\"Value .floatWrapperNull = null\",\"Open .hashMap\",\"Value .hashMap.eins = 1\",\"Value .hashMap.zwei = 2\",\"Close .hashMap\",\"Open .hashSet\",\"Value .hashSet[0] = 1\",\"Value .hashSet[1] = 4\",\"Value .hashSet[2] = 7\",\"Close .hashSet\",\"Comment .intArray.#BK[0] = simple primitive int array\",\"Open .intArray\",\"Value .intArray[0] = 1\",\"Value .intArray[1] = 2\",\"Value .intArray[2] = 3\",\"Close .intArray\",\"Value .intPrimitive = 2147483647\",\"Value .intWrapper = -2147483648\",\"Value .intWrapperNull = null\",\"Open .integerArray\",\"Comment .integerArray.#B[0] = Before\",\"Comment .integerArray.#B[1] = Before2\",\"Value .integerArray[0] = 1\",\"Value .integerArray[1] = null\",\"Value .integerArray[2] = 3\",\"Comment .integerArray.#A[0] = After\",\"Close .integerArray\",\"Open .interfaceField\",\"Value .interfaceField.bool = true\",\"Close .interfaceField\",\"Open .list\",\"Value .list[0] = eins\",\"Value .list[1] = drei\",\"Close .list\",\"Open .listWithoutGen\",\"Value .listWithoutGen[0] = eins\",\"Value .listWithoutGen[1] = drei\",\"Close .listWithoutGen\",\"Value .longPrimitive = -9223372036854775808\",\"Value .longWrapper = 9223372036854775807\",\"Value .longWrapperNull = null\",\"Open .map\",\"Value .map.eins = 1\",\"Value .map.zwei = 2\",\"Close .map\",\"Open .mapWithoutGen\",\"Value .mapWithoutGen.eins = 1\",\"Value .mapWithoutGen.zwei = 2\",\"Close .mapWithoutGen\",\"Value .nullList = null\",\"Value .nullMap = null\",\"Value .nullSet = null\",\"Open .objectHashSet\",\"Value .objectHashSet[0] = 1\",\"Value .objectHashSet[1] = 4\",\"Value .objectHashSet[2] = 7\",\"Close .objectHashSet\",\"Value .objectWithByte = 127\",\"Value .objectWithFloat = 1.4E-45\",\"Open .set\",\"Value .set[0] = 1\",\"Value .set[1] = 4\",\"Value .set[2] = 7\",\"Close .set\",\"Open .setWithoutGen\",\"Value .setWithoutGen[0] = 1\",\"Value .setWithoutGen[1] = 4\",\"Value .setWithoutGen[2] = 7\",\"Close .setWithoutGen\",\"Value .shortNull = null\",\"Value .shortPrimitive = 32767\",\"Value .shortWrapper = -32768\",\"Value .stringField = fsdajbfdshbdfs98h\",\"Value .stringFieldNull = null\",\"Comment .timespan.#BK[0] = A timespan instance defines an interval or a timeperiod. Available Units: [+-]*Y(ears)*M(onths)*W(eeks)*D(ays)*h(ours)*m(inutes)*s(seconds)*S(milliSeconds)\\r\\n.Each unit is optional, but there has to be at least one.\",\"Value .timespan = 10h15m\",\"Value .booleanPrimitive = false\",\"Close .\"]",
                        TypeRef.STRING_LIST);
        FlexiJSonObject node = (FlexiJSonObject) mapper.objectToJsonNode(new ObjectContainsAll());
        node.getNode("integerArray").addCommentsBefore(new FlexiJSonComments(new FlexiComment("Before", org.appwork.storage.flexijson.FlexiComment.Type.INLINE, FlexiMapperTags.DOCS), new FlexiComment("Before2", org.appwork.storage.flexijson.FlexiComment.Type.INLINE, FlexiMapperTags.DOCS)));
        node.getNode("integerArray").addCommentsAfter(new FlexiJSonComments(new FlexiComment("After", org.appwork.storage.flexijson.FlexiComment.Type.INLINE, FlexiMapperTags.DOCS)));
        FlexiJSonNode before = node.getNode("integerArray").getCommentsBefore();
        final ArrayList<String> list = new ArrayList<String>();
        FlexiUtils.walk(node, new FlexiVisitor() {
            @Override
            public void openObject(FlexiJSonObject obj, JSPath path) {
                list.add("Open " + path.toPathString(true));
            }

            @Override
            public void openArray(FlexiJSonArray array, JSPath path) {
                list.add("Open " + path.toPathString(true));
            }

            @Override
            public void onValue(FlexiJSonValue value, JSPath path) {
                list.add("Value " + path.toPathString(true) + " = " + value.getValue());
            }

            @Override
            public void onComment(FlexiCommentJsonNode comment, JSPath path) {
                list.add("Comment " + path.toPathString(true) + " = " + ((FlexiComment) comment).getText());
            }

            @Override
            public void closeObject(FlexiJSonObject obj, JSPath path) {
                list.add("Close " + path.toPathString(true));
            }

            @Override
            public void closeArray(FlexiJSonArray array, JSPath path) {
                list.add("Close " + path.toPathString(true));
            }
        });
        System.out.println(FlexiUtils.serializeMinimizedWithWTF(list));
        assertEquals(expect.size(), list.size());
        for (int i = 0; i < expect.size(); i++) {
            assertEquals(expect.get(i), list.get(i));
        }
    }

    public static void main(String[] args) {
        run();
    }
}
