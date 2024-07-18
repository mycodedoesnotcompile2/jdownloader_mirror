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

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableConditionalType;
import org.appwork.storage.StorableConditionalType2;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 06.07.2022
 *
 */
public class FlexiConditionalTypeTest extends AWTest {
    public static class OptionA implements Storable {
        /**
         *
         */
        public OptionA() {
        }

        private boolean testa;

        public boolean isTesta() {
            return testa;
        }

        public void setTesta(boolean testa) {
            this.testa = testa;
        }
    }

    public static class OptionB implements Storable {
        /**
        *
        */
        public OptionB() {
        }

        private boolean testb;

        public boolean isTestb() {
            return testb;
        }

        public void setTestb(boolean testb) {
            this.testb = testb;
        }
    }

    public static class Example implements Storable {
        /**
         *
         */
        public Example() {
        }

        private int a = 0;

        public int getA() {
            return a;
        }

        public void setA(int a) {
            this.a = a;
        }

        public Object getO() {
            return o;
        }

        public void setO(Object o) {
            this.o = o;
        }

        @StorableConditionalType(condition = "{a:1}", cls = OptionA.class)
        @StorableConditionalType2(condition = "{a:2}", cls = OptionB.class)
        @AllowNonStorableObjects
        private Object o;
    }

    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        Example example1 = new FlexiJSonMapper().jsonToObject(new FlexiJSONParser("{a:1,o:{}}").ignoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).parse(), new SimpleTypeRef<Example>(Example.class));
        Example example2 = new FlexiJSonMapper().jsonToObject(new FlexiJSONParser("{a:2,o:{}}").ignoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).parse(), new SimpleTypeRef<Example>(Example.class));
        assertTrue(example1.getO() instanceof OptionA);
        assertTrue(example2.getO() instanceof OptionB);
    }
}
