package org.appwork.moncompare.tests;

import static org.appwork.moncompare.Condition.C;

import java.util.ArrayList;
import java.util.Arrays;

import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.Scope;
import org.appwork.moncompare.TypeHandler;
import org.appwork.moncompare.fromjson.FlexiCondition;
import org.appwork.moncompare.typehandler.FlexiTypeHandler;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.testframework.AWTest;

public class ConditionsOnFlexiTest extends AWTest {
    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public void runTest() throws Exception {
        Condition.THREAD_DEBUG.set(true);
        Condition.THREAD_LOGGER.set(LogV3.defaultLogger());
        final FlexiJSonNode flexi = new FlexiJSONParser("{\"string\":\"This is a JSON string\",\"number\":42,\"boolean\":true,\"nullValue\":null,\"undefinedValue\":null,\"array\":[9,8,7],\"multidimensionalArray\":[[1,2,3],[4,5,6]],\"map\":{\"key1\":\"value1\",\"key2\":\"value2\"}}").parse();
        Condition.TYPE_HANDLERS.set(new ArrayList<TypeHandler>(Arrays.asList(new FlexiTypeHandler())));
        final Scope keys = new Condition().resolveKeyPath(new Scope(flexi), JSPath.fromPathString("§keys"));
        assertEquals(new Condition("§sum", new Object[] { "§number", 2 }).evaluate(flexi), 44);
        // number is ignored
        assertEquals(new Condition("§sum", new Object[] { "number", 2 }).evaluate(flexi), 2);
        // 100 is in a sublist and must be ignored
        assertEquals(new Condition("§sum", new Object[] { 0, 1, 3, Arrays.asList(100) }).evaluate(flexi), 4);
        // [1,100] is the only entry and must be summed
        assertEquals(new Condition("§sum", new Object[] { Arrays.asList(1, 100) }).evaluate(flexi), 101);
        assertEquals(new Condition("§sum", new Object[] { "§number", "§number" }).evaluate(flexi), 84);
        assertEquals(new Condition("§sum", new Object[] { "§number", "§multidimensionalArray[0][0]" }).evaluate(flexi), 43);
        // final String expect = Deser.toString(keys.getLast(), SC.HASH_CONTENT);
        assertEqualsDeep(keys.getLast(), new String[] { "string", "number", "boolean", "nullValue", "undefinedValue", "array", "multidimensionalArray", "map" });
        assertTrue(new Condition("number", 42).matches(flexi));
        assertTrue(new Condition<FlexiJSonNode>("number", new Condition("§gt", 10)).matches(flexi));
        final Condition numberGt10 = new Condition("number", new Condition("§gt", 10));
        assertTrue(new Condition("boolean", new FlexiJSonValue(true)).matches(flexi));
        final Condition booleanTrue = new Condition("boolean", true);
        assertTrue(new Condition("§and", Arrays.asList(numberGt10, booleanTrue)).matches(flexi));
        assertFalse(new Condition("§and", Arrays.asList(numberGt10, new Condition("§not", booleanTrue))).matches(flexi));
        assertTrue(new Condition("§or", Arrays.asList(numberGt10, new Condition("§not", booleanTrue))).matches(flexi));
        assertTrue(C("string", C("§regex", ".*JSON\\s+string.*")).matches(flexi));
        assertFalse(C("string", C("§regex", ".*JSO\\s+string.*")).matches(flexi));
        assertEquals(new Condition("§subtract", new Object[] { "§number", 2 }).evaluate(flexi), 40);
        // 7 is in array
        assertTrue(FlexiCondition.parse("{array:{§in:[7]}}").matches(flexi));
        // 7 and 8 is in array
        assertTrue(FlexiCondition.parse("{array:{§in:[7,8]}}").matches(flexi));
        // 1 is not in array
        assertFalse(FlexiCondition.parse("{array:{§in:[1]}}").matches(flexi));
        // 1 is in array
        assertTrue(FlexiCondition.parse("{array:{§in:[1,7]}}").matches(flexi));
        // number is 42
        assertTrue(FlexiCondition.parse("{number:{§in:[1,7,42]}}").matches(flexi));
        // 42 is not in the condition
        assertFalse(FlexiCondition.parse("{number:{§in:[1,7]}}").matches(flexi));
        assertEquals(new Condition("§multiply", new Object[] { "§number", 2 }).evaluate(flexi), 84);
    }

    public static void main(final String[] args) {
        run();
    }
}
