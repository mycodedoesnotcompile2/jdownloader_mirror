/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.formatter.test;

import java.util.Date;
import java.util.Locale;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.formatter.TimeFormatter;

/**
 * @author daniel
 * @date Feb 13, 2025
 *
 */
@TestDependency({ "org.appwork.utils.formatter.TimeFormatter" })
public class TimeFormatterTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        final Date sep = TimeFormatter.parseDateString("Sat, 28 Sep 2024 09:57:00 GMT");
        assertNotNull(sep);
        final Date sept = TimeFormatter.parseDateString("Sat, 28 Sept 2024 09:57:00 GMT");
        assertNotNull(sept);
        assertEquals(sep.getTime(), sept.getTime());
        long septUK = TimeFormatter.getMilliSeconds("Sat, 28 Sept 2024 09:57:00 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz", Locale.UK);
        assertEqualsNot(septUK, -1l);
        long septEnglish = TimeFormatter.getMilliSeconds("Sat, 28 Sept 2024 09:57:00 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH);
        assertEqualsNot(septEnglish, -1l);
        assertEquals(septUK, septEnglish);
    }
}
