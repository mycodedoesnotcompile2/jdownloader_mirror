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

import java.util.ArrayList;
import java.util.Arrays;

import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 * @date 28.07.2023
 *
 */
public class GenList2DTestForSImpleJson extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    public static enum TEstENum {
        A,
        B
    }

    @Override
    public void runTest() throws Exception {
        {
            // primitives work, because simplemapper can "guess" them
            GenericListTypeTestClass<Integer> ist = new GenericListTypeTestClass<Integer>();
            ist.setList2D(new ArrayList<ArrayList<Integer>>(Arrays.asList(new ArrayList<Integer>(Arrays.asList(1, 2, 3)), new ArrayList<Integer>(Arrays.asList(1, 2, 3)))));
            SimpleMapper mapper = new SimpleMapper();
            String json = mapper.objectToString(ist);
            GenericListTypeTestClass<Integer> restored = mapper.stringToObject(json, new TypeRef<GenericListTypeTestClass<Integer>>() {
            });
            assertEqualsDeep(ist, restored);
        }
        {
            GenericListTypeTestClass<Double> ist = new GenericListTypeTestClass<Double>();
            ist.setList2D(new ArrayList<ArrayList<Double>>(Arrays.asList(new ArrayList<Double>(Arrays.asList(1d, 2d, 3d)), new ArrayList<Double>(Arrays.asList(1d, 2d, 3d)))));
            SimpleMapper mapper = new SimpleMapper();
            String json = mapper.objectToString(ist);
            GenericListTypeTestClass<Double> restored = mapper.stringToObject(json, new TypeRef<GenericListTypeTestClass<Double>>() {
            });
            assertEqualsDeep(ist, restored);
        }
        // {
        // // Enums and all non-primitivs will fail
        // // Was tun?
        // GenericListTypeTestClass<TEstENum> ist = new GenericListTypeTestClass<TEstENum>();
        // ist.setList2D(new ArrayList<ArrayList<TEstENum>>(Arrays.asList(new ArrayList<TEstENum>(Arrays.asList(TEstENum.A, TEstENum.B)),
        // new ArrayList<TEstENum>(Arrays.asList(TEstENum.B, TEstENum.A)))));
        // SimpleMapper mapper = new SimpleMapper();
        // String json = mapper.objectToString(ist);
        // GenericListTypeTestClass<TEstENum> restored = mapper.stringToObject(json, new TypeRef<GenericListTypeTestClass<TEstENum>>() {
        // });
        // assertEqualsDeep(ist, restored);
        // }
    }

    public static void main(String[] args) {
        run();
    }
}
