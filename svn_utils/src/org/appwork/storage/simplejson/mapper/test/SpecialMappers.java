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

import java.awt.Color;
import java.io.File;
import java.net.URL;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.storage.simplejson.mapper.ColorMapper;
import org.appwork.storage.simplejson.mapper.FileMapper;
import org.appwork.storage.simplejson.mapper.URLMapper;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.duration.TimeSpan;

/**
 * @author thomas
 *
 */
public class SpecialMappers extends AWTest {
    public static void main(final String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        JSonStorage.setMapper(new SimpleMapper());
        ((SimpleMapper) JSonStorage.getMapper()).getMapper().addMapper(Color.class, new ColorMapper());
        ((SimpleMapper) JSonStorage.getMapper()).getMapper().addMapper(File.class, new FileMapper());
        ((SimpleMapper) JSonStorage.getMapper()).getMapper().addMapper(URL.class, new URLMapper());
        final String str = JSonStorage.serializeToJson(new SpecialObject(Application.getResource("list.txt"), SpecialMappers.class.getResource("SpecialMappers.class"), TimeSpan.parse("2W1D")));
        final SpecialObject obj = JSonStorage.restoreFromString(str, new TypeRef<SpecialObject>() {
        }, null);
        final String str2 = JSonStorage.serializeToJson(obj);
        if (str2.equals(str)) {
            LogV3.info("OK");
        } else {
            LogV3.info("FAILED");
            LogV3.info(str);
            LogV3.info(str2);
        }
        assertEqualsDeep(str, str2);
    }
}
