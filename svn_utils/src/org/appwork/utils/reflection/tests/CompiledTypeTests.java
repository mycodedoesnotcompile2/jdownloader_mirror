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
package org.appwork.utils.reflection.tests;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

import org.appwork.moncompare.Condition;
import org.appwork.storage.TypeRef;
import org.appwork.storage.simplejson.MinimalMemoryMap;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.testframework.AWTest;
import org.appwork.txtresource.LocaleMap;
import org.appwork.utils.KeyValueEntry;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.CompiledType.AbstractNameScheme;
import org.appwork.utils.reflection.CompiledType.PrimitiveWrapperStrategy;
import org.appwork.utils.reflection.JavaSyntax;
import org.appwork.utils.reflection.JsonSyntax;

/**
 * @author thomas
 * @param <T>
 * @date 17.10.2022
 *
 */
public class CompiledTypeTests<T extends Object> extends AWTest {
    public static enum MyEnum {
        A,
        B
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        ClassCache cc = ClassCache.getClassCache(A.class);
        assertTrue(cc.getAllGetter().size() == 1);
        final AbstractNameScheme javaRule = new JavaSyntax();
        final AbstractNameScheme jsonRule = new JsonSyntax();
        assertEquals("Array<Condition<Object>>", CompiledType.create(new TypeRef<ArrayList<Condition<Object>>>() {
        }.getType()).toString(new JsonSyntax()));
        // Condition
        CompiledType ct = CompiledType.create(Condition.class);
        assertEquals("Condition<Object>", ct.toString(javaRule));
        assertEquals("Condition<Object>", ct.toString(jsonRule));
        // MyEnum
        ct = CompiledType.create(MyEnum.class);
        assertTrue(ct.componentTypes.length == 0);
        assertEquals("MyEnum", ct.toString(javaRule));
        ct = CompiledType.create(new CompiledTypeTests() {
        }.getClass());
        assertEquals("", ct.toString(null));
        assertEquals("CompiledTypeTests<Object>", ct.toString(new JavaSyntax().skipAnonymousClasses(true)));
        // int[]
        ct = CompiledType.create(int[].class);
        assertEquals("Array<int>", ct.toString(jsonRule));
        assertEquals("int[]", ct.toString(javaRule));
        assertEquals("Array<Integer>", ct.toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.WRAPPER_NAMES_FOR_BOTH)));
        assertEquals("Array<int>", ct.toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.DEDICATED_NAME_FOR_EACH)));
        // Integer[]
        ct = CompiledType.create(Integer[].class);
        assertEquals("Array<Integer>", ct.toString(jsonRule));
        assertEquals("Integer[]", ct.toString(javaRule));
        assertEquals("Array<int>", ct.toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.PRIMITIVE_NAMES_FOR_BOTH)));
        assertEquals("Array<Integer>", ct.toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.WRAPPER_NAMES_FOR_BOTH)));
        assertEquals("Array<Integer>", ct.toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.DEDICATED_NAME_FOR_EACH)));
        assertEquals("Array<KeyValueStringEntry>", CompiledType.create(KeyValueStringEntry[].class).toString(new JsonSyntax().primitiveWrapperStrategy(PrimitiveWrapperStrategy.DEDICATED_NAME_FOR_EACH)));
        ct = CompiledType.create(new TypeRef<KeyValueEntry<String, String>[]>() {
        }.getType());
        assertEquals("Array<KeyValueEntry<String,String>>", ct.toString(null));
        assertTrue(ct.isGenericsResolved());
        ct = CompiledType.create(new TypeRef<KeyValueEntry<String, ?>[]>() {
        }.getType());
        assertFalse(ct.isGenericsResolved());
        assertEquals("Array<KeyValueEntry<String,?>>", ct.toString());
        ct = CompiledType.create(new TypeRef<KeyValueEntry<String, ? extends String>[]>() {
        }.getType());
        assertFalse(ct.isGenericsResolved());
        assertEquals("Array<KeyValueEntry<String,? extends String>>", ct.toString());
        ct = CompiledType.create(new TypeRef<KeyValueEntry<String, T>[]>() {
        }.getType());
        String name = ct.toString();
        assertFalse(ct.isGenericsResolved());
        assertEquals("Array<KeyValueEntry<String,T>>", name);
        ct = CompiledType.create(new TypeRef<KeyValueEntry<String, T>[]>() {
        }.getType(), new TypeRef<CompiledTypeTests<CompiledType>>() {
        }.getType());
        CompiledType ct2 = CompiledType.create(new TypeRef<KeyValueEntry<String, T>[]>() {
        }.getType(), new TypeRef<CompiledTypeTests<CompiledType>>() {
        }.getType());
        CompiledType ct3 = CompiledType.create(new TypeRef<KeyValueEntry<String, T>[]>() {
        }.getType(), new TypeRef<CompiledTypeTests<Object>>() {
        }.getType());
        // cache hit.
        assertEquals(ct, ct2);
        assertEqualsNot(ct, ct3);
        name = ct.toString();
        assertTrue(ct.isGenericsResolved());
        assertEquals("Array<KeyValueEntry<String,CompiledType>>", name);
        name = ct.toString(javaRule);
        assertEquals("KeyValueEntry<String,CompiledType>[]", name);
        // CopyOnWriteArrayList
        ct = CompiledType.create(new TypeRef<CopyOnWriteArrayList<String>[]>() {
        }.getType());
        assertEquals("Array<Array<String>>", ct.toString(jsonRule));
        assertEquals("CopyOnWriteArrayList<String>[]", ct.toString(javaRule));
        // ArrayList
        ct = CompiledType.create(new TypeRef<ArrayList<String>[]>() {
        }.getType());
        assertEquals("Array<Array<String>>", ct.toString(jsonRule));
        assertEquals("ArrayList<String>[]", ct.toString(javaRule));
        // LinkedList
        ct = CompiledType.create(new TypeRef<LinkedList<String>>() {
        }.getType());
        assertEquals("Array<String>", ct.toString(jsonRule));
        assertEquals("LinkedList<String>", ct.toString(javaRule));
        // MinimalMemoryMap
        ct = CompiledType.create(new TypeRef<MinimalMemoryMap<String, Integer>>() {
        }.getType());
        assertEquals("Map<String,Integer>", ct.toString(jsonRule));
        assertEquals("MinimalMemoryMap<String,Integer>", ct.toString(javaRule));
        // HashMap
        ct = CompiledType.create(new TypeRef<HashMap<String, Integer>>() {
        }.getType());
        assertEquals("Map<String,Integer>", ct.toString(jsonRule));
        assertEquals("HashMap<String,Integer>", ct.toString(javaRule));
        // LinkedHashMap
        ct = CompiledType.create(new TypeRef<LinkedHashMap<String, Integer>>() {
        }.getType());
        assertEquals("Map<String,Integer>", ct.toString(jsonRule));
        assertEquals("LinkedHashMap<String,Integer>", ct.toString(javaRule));
        // HashSet
        ct = CompiledType.create(new TypeRef<HashSet<String>>() {
        }.getType());
        assertEquals("Array<String>", ct.toString(jsonRule));
        assertEquals("HashSet<String>", ct.toString(javaRule));
        // CopyOnWriteArraySet
        ct = CompiledType.create(new TypeRef<CopyOnWriteArraySet<String>>() {
        }.getType());
        assertEquals("Array<String>", ct.toString(jsonRule));
        assertEquals("CopyOnWriteArraySet<String>", ct.toString(javaRule));
        // LocaleMap
        ct = CompiledType.create(new TypeRef<LocaleMap>() {
        }.getType());
        assertEquals("LocaleMap", ct.toString(jsonRule));
        assertEquals("LocaleMap", ct.toString(javaRule));
        // ExtendedArrayList
        ct = CompiledType.create(new TypeRef<ExtendedArrayList>() {
        }.getType());
        assertEquals("Array<String>", ct.toString(jsonRule));
        assertEquals("ExtendedArrayList", ct.toString(javaRule));
        // ExtendedHashSet
        ct = CompiledType.create(new TypeRef<ExtendedHashSet>() {
        }.getType());
        assertEquals("Array<String>", ct.toString(jsonRule));
        assertEquals("ExtendedHashSet", ct.toString(javaRule));
        // ReGenericExtendsMap
        ct = CompiledType.create(new TypeRef<ReGenericExtendsMap<Integer, Long>>() {
        }.getType());
        assertEquals("ReGenericExtendsMap<Integer,Long>", ct.toString(jsonRule));
        assertEquals("ReGenericExtendsMap<Integer,Long>", ct.toString(javaRule));
        // ReReGenericDefinition
        ct = CompiledType.create(new TypeRef<ReReGenericDefinition>() {
        }.getType());
        assertEquals("ReReGenericDefinition", ct.toString(jsonRule));
        assertEquals("ReReGenericDefinition", ct.toString(javaRule));
    }

    public static void main(String[] args) {
        run();
    }
}
