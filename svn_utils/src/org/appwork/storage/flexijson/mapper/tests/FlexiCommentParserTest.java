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

import java.text.ParseException;

import org.appwork.serializer.SC;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiSerializer;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.FlexiVisitorAdapter;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 25.06.2021
 *
 */
public class FlexiCommentParserTest extends AWTest {
    public static void main(String[] args) throws FlexiMapperException, FlexiParserException, ParseException {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        {
            FlexiJSonNode result = new FlexiJSONParser("{/*inside*/}").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertEquals(res, "{/*inside*/}");
            assertEquals(collectPaths(result), "; .=Object; .#I[0]=Comment(inside)");
        }
        {
            FlexiJSonNode result = new FlexiJSONParser("{/*inside*/a:1}").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertTrue(res.equals("{/*inside*/\"a\":1}"));
            assertEquals(collectPaths(result), "; .=Object; .a.#BK[0]=Comment(inside); .a=Value");
        }
        {
            FlexiJSonNode result = new FlexiJSONParser("/*before*/{/*bk*/\"a\"/*ak*/:/*bv*/1/*av*/}/*after*/").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertTrue(res.equals("/*before*/{/*bk*/\"a\"/*ak*/:/*bv*/1/*av*/}/*after*/"));
            assertEquals(collectPaths(result), "; .=Object; .#B[0]=Comment(before); .a.#BK[0]=Comment(bk); .a.#B[0]=Comment(bv); .a=Value; .a.#A[0]=Comment(av); .a.#AK[0]=Comment(ak); .#A[0]=Comment(after)");
        }
        {
            FlexiJSonNode result = new FlexiJSONParser("/*before*//*before2*/{/*bk*/\"a\"/*ak*/:/*bv*/1/*av*/}/*after*/").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertTrue(res.equals("/*before*//*before2*/{/*bk*/\"a\"/*ak*/:/*bv*/1/*av*/}/*after*/"));
            assertEquals(collectPaths(result), "; .=Object; .#B[0]=Comment(before); .#B[1]=Comment(before2); .a.#BK[0]=Comment(bk); .a.#B[0]=Comment(bv); .a=Value; .a.#A[0]=Comment(av); .a.#AK[0]=Comment(ak); .#A[0]=Comment(after)");
        }
        {
            FlexiJSonNode result = new FlexiJSONParser("/*before*//*before2*/[/*b*/1,2/*a*/,3]/*after*/").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertTrue(res.equals("/*before*//*before2*/[/*b*/1,2/*a*/,3]/*after*/"));
            assertEquals(collectPaths(result), "; .=Array; .#B[0]=Comment(before); .#B[1]=Comment(before2); [0].#B[0]=Comment(b); [0]=Value; [1]=Value; [1].#A[0]=Comment(a); [2]=Value; .#A[0]=Comment(after)");
        }
        {
            FlexiJSonNode result = new FlexiJSONParser("{/*inside_start*/a:1/*inside_end*/}").setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).setDebug(new StringBuilder()).parse();
            String res = new FlexiSerializer().toString(result, SC.HASH_CONTENT);
            assertTrue(res.equals("{/*inside_start*/\"a\":1/*inside_end*/}"));
            assertEquals(collectPaths(result), "; .=Object; .a.#BK[0]=Comment(inside_start); .a=Value; .a.#A[0]=Comment(inside_end)");
        }
        System.out.println(1);
    }

    protected String collectPaths(FlexiJSonNode result) {
        final StringBuilder sb = new StringBuilder();
        FlexiUtils.walk(result, new FlexiVisitorAdapter() {
            @Override
            public void onComment(FlexiCommentJsonNode comment, JSPath path) {
                sb.append("; " + path.toPathString(true) + "=Comment(" + ((FlexiComment) comment).getText() + ")");
            }

            @Override
            public void onValue(FlexiJSonValue value, JSPath path) {
                sb.append("; " + path.toPathString(true) + "=Value");
            }

            @Override
            public void openObject(FlexiJSonObject obj, JSPath path) {
                sb.append("; " + path.toPathString(true) + "=Object");
            }

            @Override
            public void openArray(FlexiJSonArray array, JSPath path) {
                sb.append("; " + path.toPathString(true) + "=Array");
            }
        });
        return sb.toString();
    }
}
