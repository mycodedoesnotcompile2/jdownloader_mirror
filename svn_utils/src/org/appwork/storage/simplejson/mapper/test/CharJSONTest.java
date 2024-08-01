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
package org.appwork.storage.simplejson.mapper.test;

import java.util.HashMap;
import java.util.Map;

import org.appwork.storage.JSONMapper;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.Storable;
import org.appwork.storage.simplejson.ParserException;
import org.appwork.storage.simplejson.mapper.MapperException;
import org.appwork.testframework.AWTest;

/**
 * @author daniel
 * @date Jul 30, 2024
 *
 */
public class CharJSONTest extends AWTest {
    public static class TestObject implements Storable {
        private char value;

        public char getValue() {
            return value;
        }

        public void setValue(char value) {
            this.value = value;
        }
    }

    public static void main(final String[] args) throws MapperException, ParserException {
        run();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        final SimpleMapper mapper = new SimpleMapper();
        runTest(mapper);
    }

    public void runTest(JSONMapper mapper) throws Exception {
        TestObject input = new TestObject();
        input.setValue('\u25cb');
        String jsonString = mapper.objectToString(input);
        TestObject output = mapper.stringToObject(jsonString, TestObject.class);
        assertEquals(input.getValue(), output.getValue());
        {
            // char to char
            Map<String, Object> map = new HashMap<String, Object>();
            map.put("value", input.getValue());
            jsonString = mapper.objectToString(map);
            output = mapper.stringToObject(jsonString, TestObject.class);
            assertEquals(input.getValue(), output.getValue());
        }
        {
            // string to char
            Map<String, Object> map = new HashMap<String, Object>();
            map.put("value", String.valueOf(input.getValue()));
            mapper.objectToString(input);
            output = mapper.stringToObject(jsonString, TestObject.class);
            assertEquals(input.getValue(), output.getValue());
        }
        {
            // number to char
            Map<String, Object> map = new HashMap<String, Object>();
            map.put("value", Character.codePointAt(String.valueOf(input.getValue()), 0));
            mapper.objectToString(input);
            output = mapper.stringToObject(jsonString, TestObject.class);
            assertEquals(input.getValue(), output.getValue());
        }
    }
}
