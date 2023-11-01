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

import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IDETestRunner;

/**
 * @author thomas
 * @date 26.03.2021
 *
 */
public class DateTest extends AWTest {
    public static void main(String[] args) throws FlexiParserException, UnsupportedEncodingException, InterruptedException {
        IDETestRunner.run(DateTest.class);
        LogV3.disableSysout();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        // yyyy-MM-dd'T'HH:mm:ss.SSSz
        assertEquals(1665672600000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50:00.000+0100\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertEquals(1665672600000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50:00+0100\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertEquals(1665672600000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50+0100\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertEquals(1665672600000l - 60 * 60000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50:00.000\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertEquals(1665672600000l - 60 * 60000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50:00\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertEquals(1665672600000l - 60 * 60000l, FlexiUtils.jsonToObject("\"2022-10-13T15:50\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
    }
}
