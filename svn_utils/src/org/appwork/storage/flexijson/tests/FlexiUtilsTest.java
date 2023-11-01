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

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 06.07.2022
 *
 */
public class FlexiUtilsTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @StorableDoc("Enum Class Anno")
    public static enum TestEnum {
        @StorableDoc("A comment")
        A,
        @StorableDoc("b comment")
        B
    }

    public static class PropTest implements Storable {
        /**
         *
         */
        public PropTest() {
            // TODO Auto-generated constructor stub
        }

        private TestEnum en = TestEnum.A;

        @StorableDoc("Property Storable")
        public TestEnum getEn() {
            return en;
        }

        public void setEn(TestEnum en) {
            this.en = en;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        // if we ignore duplicate key errors, we should have all elements in the object
        FlexiJSonObject node = (FlexiJSonObject) new FlexiJSONParser("{a:true,a:false}").ignoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).addIgnoreIssues(ParsingError.ERROR_DUPLICATED_KEY_PROPERTY).parse();
        assertThat(node.size()).isNumber(2);
        String result = new FlexiJSonStringBuilder().toJSONString(node);
        assertEquals("{\"a\":true,\"a\":false}", result);
        assertEquals(FlexiUtils.serializeConfigStorable(TestEnum.A), "\"A\"/* A comment */");
        assertEquals(FlexiUtils.serializeConfigStorable(new PropTest()), "{\r\n  /*\r\n   * Enum Class Anno\r\n   * Property Storable\r\n   * Type: TestEnum-Enum\r\n   * Options: \r\n   *    A: A comment\r\n   *    B: b comment\r\n   */\r\n  \"en\":\"A\"/* A comment */\r\n}");
    }
}
