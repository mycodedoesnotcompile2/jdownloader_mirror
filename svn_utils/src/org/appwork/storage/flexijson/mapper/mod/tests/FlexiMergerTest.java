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
package org.appwork.storage.flexijson.mapper.mod.tests;

import java.util.Arrays;
import java.util.HashSet;

import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.ConditionException;
import org.appwork.moncompare.fromjson.FlexiCondition;
import org.appwork.moncompare.typehandler.FlexiTypeHandler;
import org.appwork.serializer.Deser;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiSerializer;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.mod.FlexiModifier;
import org.appwork.storage.flexijson.mapper.mod.IllegalPathException;
import org.appwork.storage.flexijson.mapper.mod.JsonModification;
import org.appwork.storage.flexijson.mapper.mod.MergeException;
import org.appwork.storage.flexijson.mapper.mod.SetHandler;
import org.appwork.storage.flexijson.mapper.mod.UnsetHandler;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 07.07.2023
 *
 */
public class FlexiMergerTest extends AWTest {
    @SuppressWarnings("unchecked")
    @Override
    public void runTest() throws Exception {
        SerializerInterface restore = Deser.setThreadDeser(new FlexiSerializer());
        FlexiTypeHandler removeTypeHandler = Condition.addThreadTypeHandler(new FlexiTypeHandler());
        Condition.THREAD_DEBUG.set(true);
        Condition.THREAD_LOGGER.set(LogV3.defaultLogger());
        try {
            FlexiJSonObject base = new FlexiJSonObject();
            FlexiModifier<FlexiJSonObject, Boolean> mod = new FlexiModifier<FlexiJSonObject, Boolean>(base);
            JsonModification<FlexiJSonObject, Boolean> mods = new JsonModification<FlexiJSonObject, Boolean>();
            FlexiJSonObject set = new FlexiJSonObject();
            set.put("a.b.c", 1);
            set.put("a.array[1]", 1);
            set.put("a.array[3]", 3);
            set.put("bool", true);
            mods.setSet(set);
            Condition.putThreadOPHandler("§set", new SetHandler());
            Condition.putThreadOPHandler("§unset", new UnsetHandler());
            FlexiJSonObject base2 = new FlexiJSonObject();
            // FlexiCondition.parse("{a:{§set:1}}").matches(base2);
            // FlexiCondition.parse("{§unset:[a]}").matches(base2);
            {
                // set in path resolves to its own root -> a
                assertEquals(Boolean.TRUE, FlexiCondition.parse("{\"a[{§set:{b:1}}]\":true,§options:{create:true}}").evaluate(base2));
                assertEquals("{\"a\":{\"b\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
                assertEquals(Boolean.FALSE, FlexiCondition.parse("{\"a[{§set:{b:1}}]\":true,§options:{create:true}}").evaluate(base2));
                assertEquals("{\"a\":{\"b\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            // assertEquals(Boolean.TRUE, FlexiCondition.parse("{a:{§set:null}}").evaluate(base2));
            assertEquals(Boolean.TRUE, FlexiCondition.parse("{a.b:{§set:null}}").evaluate(base2));
            assertEquals("{\"a\":{\"b\":null}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            assertEquals(Boolean.TRUE, FlexiCondition.parse("{a.b:{§set:1}}").evaluate(base2));
            assertEquals("{\"a\":{\"b\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            base2 = new FlexiJSonObject();
            {
                FlexiCondition.parse("{a.array:{§set:{[1]:4}},§options:{create:true}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4]}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                FlexiCondition.parse("{a.b:{§set:{c.d:1,array:[]}},§options:{create:true}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":{\"c\":{\"d\":1},\"array\":[]}}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                // try again - same result expected
                FlexiCondition.parse("{a.b:{§set:{c.d:1,array:[]}},§options:{create:true}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":{\"c\":{\"d\":1},\"array\":[]}}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                FlexiCondition.parse("{a.b.array[5].inArray:{§set:{§§this:1}},§options:{create:true}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":{\"c\":{\"d\":1},\"array\":[null,null,null,null,null,{\"inArray\":1}]}}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                FlexiCondition.parse("{a.b.array:{§set:{§§this[3]:true}},§options:{create:true}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":{\"c\":{\"d\":1},\"array\":[null,null,null,true,null,{\"inArray\":1}]}}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                FlexiCondition.parse("{a.b:{§set:1}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                Object reta = FlexiCondition.parse("{§and:[{a.c:{§exists:false}},{a.c:{§set:1}}]}").evaluate(base2);
                Object retb = FlexiCondition.parse("{§and:[{a.c:{§exists:false}},{a.c:{§set:2}}]}").evaluate(base2);
                // FlexiCondition.parse("{§and:[{a.c:1},{a.c:{§set:2}}]}").evaluate(base2);
                // FlexiCondition.parse("{a.c:{§set:1,$options:{ifUnset:true}}}").evaluate(base2);
                // FlexiCondition.parse("{a.b:{§set:2,$options:{ifUnset:true}}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":1,\"c\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                try {
                    FlexiCondition.parse("{a.b.c.d:{§set:1}}").evaluate(base2);
                    throw new Exception("ConditionException expected");
                } catch (ConditionException e) {
                    // expected
                }
            }
            {
                FlexiCondition.parse("{a.b:{§set:{§PARENT.b:3}}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4],\"b\":3,\"c\":1}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            {
                FlexiCondition.parse("{a.c:{§unset:[§§THIS,§PARENT.b]}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[null,4]}}", FlexiUtils.serializeMinimizedWithWTF(base2));
                FlexiCondition.parse("{a.array:{§unset:[0]}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[4]}}", FlexiUtils.serializeMinimizedWithWTF(base2));
                FlexiCondition.parse("{a.array:{§unset:[\"[{§project:{§§THIS:4}}].§first\"]}}").evaluate(base2);
                assertEquals("{\"a\":{\"array\":[]}}", FlexiUtils.serializeMinimizedWithWTF(base2));
            }
            mod.setAutoCreateStructures(false);
            try {
                mod.merge(mods);
                throw new Exception("IllegalPathException expected");
            } catch (IllegalPathException e) {
                // expected
            }
            mod.setAutoCreateStructures(true);
            mod.merge(mods);
            String json = FlexiUtils.serializeMinimized(base);
            assertEquals("{\"a\":{\"b\":{\"c\":1},\"array\":[null,1,null,3]},\"bool\":true}", json);
            {
                set = new FlexiJSonObject();
                set.put("a.array[{§project:{§§this:1}}].§first", 3);
                set.put("a[{§project:{c:{§exists:true}}}].§first.value", true);
                mods.setSet(set);
                mod.merge(mods);
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"c\":1,\"value\":true},\"array\":[null,3,null,3]},\"bool\":true}", json);
            }
            {
                set = new FlexiJSonObject();
                set.put("[0]", true);
                mods.setSet(set);
                mod.merge(mods);
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"c\":1,\"value\":true},\"array\":[null,3,null,3]},\"bool\":true,\"0\":true}", json);
                System.out.println(1);
            }
            {
                set = new FlexiJSonObject();
                set.put("a.array[key]", true);
                mods.setSet(set);
                try {
                    mod.merge(mods);
                    throw new Exception("IllegalPathException expected");
                } catch (MergeException e) {
                    // expected
                }
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"c\":1,\"value\":true},\"array\":[null,3,null,3]},\"bool\":true,\"0\":true}", json);
            }
            {
                set = new FlexiJSonObject();
                set.put("0", true);
                mods.setSet(set);
                mod.merge(mods);
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"c\":1,\"value\":true},\"array\":[null,3,null,3]},\"bool\":true,\"0\":true}", json);
            }
            {
                set = new FlexiJSonObject();
                mods.setSet(set);
                mods.setUnset(new HashSet<String>(Arrays.asList("a.b.c", "a.b.array[0]", "a.array[0]")));
                mod.merge(mods);
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"value\":true},\"array\":[3,null,3]},\"bool\":true,\"0\":true}", json);
            }
            {
                set = new FlexiJSonObject();
                set.put("newEntry", "test");
                set.put("a", null);
                set.put("a.b.newEntry2", "yeah");
                mods.setSet(null);
                mods.setUnset(null);
                mods.setSetIfUnset(set);
                mod.merge(mods);
                json = FlexiUtils.serializeMinimized(base);
                assertEquals("{\"a\":{\"b\":{\"value\":true,\"newEntry2\":\"yeah\"},\"array\":[3,null,3]},\"bool\":true,\"0\":true,\"newEntry\":\"test\"}", json);
            }
            System.out.println(json);
        } finally {
            Deser.setThreadDeser(restore);
            Condition.removeThreadTypeHandler(removeTypeHandler);
            Condition.removeThreadOPHandler("§set");
            Condition.removeThreadOPHandler("§unset");
            Condition.THREAD_DEBUG.set(false);
            Condition.THREAD_LOGGER.set(null);
        }
    }

    public static void main(String[] args) {
        run();
    }
}
