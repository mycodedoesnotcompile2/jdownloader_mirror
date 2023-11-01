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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.simplejson.mapper.test;

import java.io.UnsupportedEncodingException;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.simplejson.JSonFactory;
import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.ParserException;
import org.appwork.storage.simplejson.mapper.JSonMapper;
import org.appwork.storage.simplejson.mapper.MapperException;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 *
 */
public class PerformanceTest extends AWTest {
    /**
     * @return
     */
    private static Object create() {
        TestClass ret = null;
        ret = TestClass.createObject();
        // ret = new TestClass();
        return ret;
    }

    public static void main(final String[] args) throws MapperException, ParserException, UnsupportedEncodingException {
        run();
    }

    /**
     * @throws Exception
     *
     */
    private static void testDeserialize(final int iterations) throws Exception {
        final Object obj = PerformanceTest.create();
        final JSonMapper mapper = new JSonMapper();
        JSonNode json = mapper.create(obj);
        long started = System.currentTimeMillis();
        double awu;
        double flexi;
        double jackson;
        final String jsonString = json.toString();
        {

            long t = System.currentTimeMillis();
            TestClass ss;
            for (int i = 0; i < iterations; i++) {
                json = new JSonFactory(jsonString).parse();
                ss = mapper.jsonToObject(json, new TypeRef<TestClass>() {
                });
                // System.out.println(node);
            }
            final long self = System.currentTimeMillis() - t;

            System.out.println("Des(awu)|" + (awu = (self / (double) iterations)) + "ms/iteration");

        }
        {

            long t = System.currentTimeMillis();
            TestClass ss;
            for (int i = 0; i < iterations; i++) {
                try {
                    FlexiJSonNode fjson = new FlexiJSONParser(jsonString).parse();

                    ss = mapper.jsonToObject(json, new TypeRef<TestClass>() {
                    });
                } catch (org.appwork.storage.flexijson.FlexiParserException e) {                    
                    e.printStackTrace();
                }
                // System.out.println(node);
            }
            final long self = System.currentTimeMillis() - t;
            System.out.println("Des(Flexi)|" + (flexi = (self / (double) iterations)) + "ms/iteration");

        }

        // Messwerte sind mit 3000 Iterationen
        // just test relations to be less dependend on different cpus and test enviroments

        double flexioAWU = 2.6243243243243244;

    }

    /**
     * @throws MapperException
     *
     */
    private static void testSerialize(final int iterations) throws MapperException {
        final Object obj = PerformanceTest.create();

        final JSonMapper mapper = new JSonMapper();

        // mapper.create(obj);
        long t = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {

            final JSonNode node = mapper.create(obj);
            node.toString();
            // System.out.println(node);
        }
        final long self = System.currentTimeMillis() - t;
        System.out.println("Ser(awu)|" + (self / (double) iterations) + "ms/iteration");

        // JSonStorage.serializeToJson(obj);
        t = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {
            JSonStorage.serializeToJson(obj);
            // System.out.println();
        }
        final long jackson = System.currentTimeMillis() - t;
        System.out.println("Ser(jackson)|" + (jackson / (double) iterations) + "ms/iteration");
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {

        // for (int i = 1; i <= 100; i++) {
        // PerformanceTest.testSerialize(i * 1);
        // PerformanceTest.testDeserialize(i * 1);
        // }

        PerformanceTest.testDeserialize(3000);
    }
}
