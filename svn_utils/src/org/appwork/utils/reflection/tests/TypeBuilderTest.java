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

import java.lang.reflect.Type;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.JavaSyntax;
import org.appwork.utils.reflection.TypeBuilder;
import org.appwork.utils.reflection.TypeParserException;

/**
 * @author thomas
 * @param <T>
 * @date 17.10.2022
 *
 */
public class TypeBuilderTest<T extends Object> extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        {
            try {
                Type type = new TypeBuilder().parse("java.util.List<java.lang.String");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("java.util.List<java.lang.String");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("java.util.List<>");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("int[][][],,");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<String,,Object>>");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            Type type = new TypeBuilder().parse("Map<String,Map<Integer,Long>>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.Map<java.lang.String,java.util.Map<java.lang.Integer,java.lang.Long>>");
        }
        {
            Type type = new TypeBuilder().parse("List<String[]>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.List<java.lang.String[]>");
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<String,Object>>");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
                // e.printStackTrace();
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<<String,Object>");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<String,");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<String");
                throw new WTFException("This should throw a exception");
            } catch (TypeParserException e) {
                // expected
            }
        }
        {
            try {
                Type type = new TypeBuilder().parse("Map<String>");
                throw new WTFException("This should throw a exception: Invalid generic definition for interface java.util.Map");
            } catch (TypeParserException e) {
                // expected
            }
        }
        {
            Type type = new TypeBuilder().parse("int[][]");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "int[][]");
        }
        {
            Type type = new TypeBuilder().parse("Integer[]");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.lang.Integer[]");
        }
        {
            Type type = new TypeBuilder().parse("LinkedHashMap<String,List<Integer>>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.LinkedHashMap<java.lang.String,java.util.List<java.lang.Integer>>");
        }
        {
            Type type = new TypeBuilder().parse("java.lang.Integer[]");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.lang.Integer[]");
        }
        {
            Type type = new TypeBuilder().parse("java.lang.String[]");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.lang.String[]");
        }
        {
            Type type = new TypeBuilder().parse("int[]");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "int[]");
        }
        {
            Type type = new TypeBuilder().parse("java.util.List<java.lang.Integer>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.List<java.lang.Integer>");
        }
        {
            Type type = new TypeBuilder().parse("java.util.Map<java.lang.String, java.util.List<java.lang.Integer>>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.Map<java.lang.String,java.util.List<java.lang.Integer>>");
        }
        {
            Type intType = new TypeBuilder().parse("int");
            assertEquals(CompiledType.create(intType).toString(new JavaSyntax(false)), "int");
            Type booleanType = new TypeBuilder().parse("boolean");
            assertEquals(CompiledType.create(booleanType).toString(new JavaSyntax(false)), "boolean");
            Type doubleType = new TypeBuilder().parse("double");
            assertEquals(CompiledType.create(doubleType).toString(new JavaSyntax(false)), "double");
        }
        {
            Type integerType = new TypeBuilder().parse("java.lang.Integer");
            assertEquals(CompiledType.create(integerType).toString(new JavaSyntax(false)), "java.lang.Integer");
            Type booleanType = new TypeBuilder().parse("java.lang.Boolean");
            assertEquals(CompiledType.create(booleanType).toString(new JavaSyntax(false)), "java.lang.Boolean");
            Type doubleType = new TypeBuilder().parse("java.lang.Double");
            assertEquals(CompiledType.create(doubleType).toString(new JavaSyntax(false)), "java.lang.Double");
        }
        {
            Type type = new TypeBuilder().parse("java.util.Map<java.lang.String, java.util.Map<java.lang.String, java.lang.Integer>>");
            assertEquals(CompiledType.create(type).toString(new JavaSyntax(false)), "java.util.Map<java.lang.String,java.util.Map<java.lang.String,java.lang.Integer>>");
        }
    }

    public static void main(String[] args) {
        run();
    }
}
