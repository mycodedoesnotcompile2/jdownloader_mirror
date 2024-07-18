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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.JSPath.MetaElement;
import org.appwork.testframework.AWTest;
import org.appwork.utils.CompareUtils;

/**
 * @author thomas
 * @date 21.07.2023
 *
 */
public class JSPathTest extends AWTest {
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
        ObjectContainsAll o = new ObjectContainsAll();
        JSPath withComment = JSPath.fromPathString("hashMap.#A");
        assertTrue(JSPath.fromPathString("hashMap.#A").getElements().get(1) instanceof MetaElement);
        assertTrue(JSPath.fromPathString("hashMap[#A]").getElements().get(1) instanceof String);
        assertEquals(JSPath.fromPathString("hashMap.eins").resolve(o), 1);
        assertEquals(JSPath.fromPathString("map[{\"§concat\":[\"§eins\",\" & \",\"§zwei\"]}]").resolve(o), "1 & 2");
        assertEqualsDeep(org.appwork.storage.flexijson.JSPath.fromPathString("objArray[1].intArray.0"), new JSPath(Arrays.asList("objArray", 1, "intArray", "0")));
        assertEqualsDeep(org.appwork.storage.flexijson.JSPath.fromPathString("genericClass[\"\\k\\\\e\\]y\"]"), new JSPath(Arrays.asList("genericClass", "k\\e]y")));
        JSPath path = JSPath.fromPathString("[0].conditions[0][\"connectSetup.applicationLaunchers[0].execute.cmd\"]");
        assertEquals(path, JSPath.fromPathElements(0, "conditions", 0, "connectSetup.applicationLaunchers[0].execute.cmd"));
        List<JSPath> toSort = Arrays.asList(JSPath.fromPathString("a.b"), JSPath.fromPathString("a.b[1]"), JSPath.fromPathString("a.b.c"));
        CompareUtils.compareNumber(new Double(1d), new Long(1));
        Collections.sort(toSort);
        ArrayList<String> stringsSorted = new ArrayList<String>();
        for (JSPath p : toSort) {
            stringsSorted.add(p.toPathString(false));
        }
        assertEqualsDeep(new String[] { "a.b", "a.b[1]", "a.b.c" }, stringsSorted);
    }
}
