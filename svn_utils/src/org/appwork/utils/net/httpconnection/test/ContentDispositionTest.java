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
package org.appwork.utils.net.httpconnection.test;

import java.io.IOException;
import java.util.ArrayList;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;

/**
 * @author daniel
 *
 */
public class ContentDispositionTest extends AWTest {
    public static void main(final String[] args) throws IOException {
        AWTest.run();
    }

    /**
     * http://greenbytes.de/tech/tc2231/
     */
    public void runTest() throws Exception {
        Application.setApplication(".test");
        final ArrayList<String[]> tests = new ArrayList<String[]>();
        if (true) {
            tests.add(new String[] { "attachment;", null });
            tests.add(new String[] { "attachment", null });
            tests.add(new String[] { "filename=test.zip", "test.zip" });
            tests.add(new String[] { "filename ='test.zip'", "test.zip" });
            tests.add(new String[] { "filename= test.zip", "test.zip" });
            tests.add(new String[] { " filename=test.zip", "test.zip" });
            tests.add(new String[] { ";filename =test.zip", "test.zip" });
            tests.add(new String[] { "filename= test.zip", "test.zip" });
            tests.add(new String[] { "attachment; filename=\"=?UTF-8?B?UHLDvGZzdW1tZW4uc2hhMQ==?=\"", "Prüfsummen.sha1" });
            tests.add(new String[] { "attachment;filename*=UTF-8''Test%20Test%20%282008%29.rar;filename=\"Test Test (2008).rar\"; ", "Test Test (2008).rar" });
            tests.add(new String[] { "attachment;filename*=UTF-8''TEST (2015).rar;filename=\"TEST (2015).rar\";", "TEST (2015).rar" });

            tests.add(new String[] { "attachment; filename=\"test.zip\"; creation-date=\"Thu, 27 Nov 2014 10:17:31 +0000\"; modification-date=\"Thu, 27 Nov 2014 10:17:31 +0000\"", "test.zip" });
            tests.add(new String[] { "attachment; filename=\"foo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; filename=\"0000000000111111111122222\"", "0000000000111111111122222" });
            tests.add(new String[] { "attachment; filename=\"00000000001111111111222222222233333\"", "00000000001111111111222222222233333" });
            // tests.add(new String[] { "attachment; filename=\"f\\oo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; filename=\"Here's a semicolon;.html\"", "Here's a semicolon;.html" });
            tests.add(new String[] { "attachment; foo=\"bar\"; filename=\"foo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; foo=\"\\\"\\\\\";filename=\"foo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; FILENAME=\"foo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; filename=foo.html", "foo.html" });
            tests.add(new String[] { "attachment; file_name=foo.html", "foo.html" });
            tests.add(new String[] { "attachment; name=foo.html", "foo.html" });
            tests.add(new String[] { "attachment; filename='foo.bar'", "foo.bar" });
            tests.add(new String[] { "attachment; filename=\"foo-ä.html\"", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename=\"foo-%41.html\"", "foo-%41.html" });
            tests.add(new String[] { "attachment; filename=\"50%.html\"", "50%.html" });
            tests.add(new String[] { "attachment; name=\"foo-%41.html\"", "foo-%41.html" });
            tests.add(new String[] { "attachment; filename=\"ä-%41.html\"", "ä-%41.html" });
            tests.add(new String[] { "attachment; filename=\"foo-%c3%a4-%e2%82%ac.html\"", "foo-%c3%a4-%e2%82%ac.html" });
            tests.add(new String[] { "attachment; filename =\"foo.html\"", "foo.html" });
            tests.add(new String[] { "attachment; xfilename=foo.html", null });
            tests.add(new String[] { "attachment; filename=\"/foo.html\"", "/foo.html" });
            tests.add(new String[] { "attachment; filename=\"\\\\foo.html\"", "_foo.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''foo-a%cc%88.html; creation-date=\"Thu, 27 Nov 2014 10:17:31 +0000\"; modification-date=\"Thu, 27 Nov 2014 10:17:31 +0000\"", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*=iso-8859-1''foo-%E4.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''foo-%c3%a4-%e2%82%ac.html", "foo-ä-€.html" });
            tests.add(new String[] { "attachment; filename*=''foo-%c3%a4-%e2%82%ac.html", null });
            tests.add(new String[] { "attachment; filename*=UTF-8''foo-a%cc%88.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename* =UTF-8''foo-a%cc%88.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*= UTF-8''foo-%c3%a4.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*= UTF-8'en-US'foo-%c3%a4.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename* =UTF-8''foo-%c3%a4.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''A-%2541.html", "A-%41.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''%5cfoo.html", "_foo.html" });
            tests.add(new String[] { "attachment; filename=\"foo-ae.html\"; filename*=UTF-8''foo-%c3%a4.html", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''foo-%c3%a4.html; filename=\"foo-ae.html\"", "foo-ä.html" });
            tests.add(new String[] { "attachment; filename*=UTF-8''A-%2541.html", "A-%41.html" });

            tests.add(new String[] { "attachment; filename=\"=?ISO-8859-1?Q?foo-=E4.html?=\"", "foo-ä.html" });// RFC2047, Q Encoding
            tests.add(new String[] { "attachment; filename=\"=?ISO-8859-1*EN?Q?foo-=E4.html?=\"", "foo-ä.html" });// RFC2047, Q Encoding +
                                                                                                                  // Language specification
            tests.add(new String[] { "attachment; filename==?ISO-8859-1?Q?foo-=E4.html?=", "foo-ä.html" });// RFC2047, Q Encoding
            tests.add(new String[] { "attachment; filename==?ISO-8859-1*EN?Q?foo-=E4.html?=", "foo-ä.html" });// RFC2047, Q Encoding +
                                                                                                              // Language specification
            tests.add(new String[] { "attachment; filename==?ISO-8859-1?Q?foo-=E4.html?=", "foo-ä.html" });// RFC2047, Q Encoding
            tests.add(new String[] { "attachment; filename==?UTF-8?B?dGVzdC56aXA=?=", "test.zip" });// RFC2047, B Encoding
            tests.add(new String[] { "attachment; filename==?UTF-8*EN?B?dGVzdC56aXA=?=", "test.zip" });// RFC2047, B Encoding + Language
                                                                                                       // specification
            tests.add(new String[] { "attachment; filename=\"=?UTF-8?B?dGVzdC56aXA=?=\"", "test.zip" });// RFC2047, B Encoding
            tests.add(new String[] { "attachment; filename=\"=?UTF-8*EN?B?dGVzdC56aXA=?=\"", "test.zip" });// RFC2047, B Encoding + Language
                                                                                                           // specification

            tests.add(new String[] { "attachment; filename*=foo.html", "foo.html" });
            tests.add(new String[] { "attachment; filename*=\"foo.html\"", "foo.html" });

            // http://test.greenbytes.de/tech/tc2231/#attwithfn2231quot, RFC2231 encoded , with double quotes around the parameter value.
            tests.add(new String[] { "attachment; filename*=\"UTF-8''foo-%c3%a4.html\"", "foo-ä.html" });
        }

        for (String test[] : tests) {
            final String result = HTTPConnectionUtils.getFileNameFromDispositionHeader(test[0]);
            try {
                assertEquals(result, test[1]);
            } catch (Exception e) {
                throw new Exception("Broken: " + test[0] + " should return: " + test[1] + " but returns: " + result, e);
            }
        }
    }
}
