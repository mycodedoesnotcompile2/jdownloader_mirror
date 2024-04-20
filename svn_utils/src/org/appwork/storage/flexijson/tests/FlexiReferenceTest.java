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

import java.io.File;
import java.io.UnsupportedEncodingException;

import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.SC;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiSerializer;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IDETestRunner;

/**
 * @author thomas
 * @date 26.03.2021
 *
 */
public class FlexiReferenceTest extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        FlexiJSONParser parser = new FlexiJSONParser("{target:1,link:${target}}");
        parser.setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES);
        parser.setParseReferencesEnabled(true);
        parser.setDebug(new StringBuilder());
        FlexiJSonNode node = parser.parse();
        FlexiJSonNode target = ((FlexiJSonObject) node).getNode("link");
        assertTrue(target instanceof FlexiJSonValue);
        assertTrue(((FlexiJSonValue) target).getType() == ValueType.REFERENCE);
        String hashContent = new FlexiSerializer().toString(node, SC.HASH_CONTENT);
        assertEquals("{\"link\":${target},\"target\":1}", hashContent);
        System.out.println(node);
    }

    public static void main(String[] args) throws FlexiParserException, UnsupportedEncodingException, InterruptedException {
        IDETestRunner.run(FlexiReferenceTest.class);
        // runWayToExtensiveWhateverTest();
        LogV3.disableSysout();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(String[] args, File workingDirectory) throws Exception {
        runTest();
    }
}
