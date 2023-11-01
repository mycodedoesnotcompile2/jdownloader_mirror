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
package org.appwork.utils.encoding.test;

import java.net.URLEncoder;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;

/**
 * @author daniel
 * @date Sep 27, 2021
 *
 */
public class URLEncodeTests extends AWTest {

    public static void main(String[] args) throws Exception {
        run();
    }

    private static void testDecoding(final String input, String expected) throws Exception {
        testDecoding(input, expected, "UTF-8");
    }

    private static void testDecoding(final String input, String expected, final String charSet) throws Exception {
        final String decoded = URLEncode.decodeURIComponent(input, charSet, false);
        if (!StringUtils.equals(decoded, expected)) {
            throw new WTFException("Input:" + input + "|Decoded:" + decoded + "|Expected:" + expected);
        }
    }

    @Override
    public void runTest() throws Exception {
        testDecoding("Hello", "Hello");
        testDecoding(URLEncoder.encode("üöäß", "UTF-8"), "üöäß");
        testDecoding("%" + URLEncoder.encode("üöäß", "UTF-16"), "%üöäß", "UTF-16");
        testDecoding("% " + URLEncoder.encode("üöäß", "UTF-8"), "% üöäß");
        testDecoding("%A" + URLEncoder.encode("üöäß", "UTF-8"), "%Aüöäß");
        testDecoding("%A%" + URLEncoder.encode("üöäß", "UTF-8"), "%A%üöäß");
        testDecoding("%A%" + URLEncoder.encode("üöäß", "UTF-8") + "%", "%A%üöäß%");
        testDecoding("%A%" + URLEncoder.encode("üöäß", "UTF-8") + "%A", "%A%üöäß%A");
        testDecoding("%A%" + URLEncoder.encode("üöäß", "UTF-32") + "%A %%", "%A%üöäß%A %%", "UTF-32");
        testDecoding("%C3%BC%%C3%B6%C3%A4%C3%9F", "ü%öäß");
        testDecoding("%C3%BC%A%C3%B6%C3%A4%C3%9F", "ü%Aöäß");
        testDecoding("%C3%BC%1%2%3%C3%B6%C3%A4%C3%9F", "ü%1%2%3öäß");
        testDecoding("Rpf4o%2Ff%2BJ0F%2FBKFLwCTvSQ%3D%3D", "Rpf4o/f+J0F/BKFLwCTvSQ==");
    }
}
