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

import org.appwork.testframework.AWTest;
import org.appwork.utils.encoding.RFC2047;

/**
 * @author daniel
 * @date Mar 29, 2022
 *
 */
public class RFC2047Test extends AWTest {

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final RFC2047 rfc2047 = new RFC2047();
        boolean relaxed = true;
        do {
            assertEquals("If you can read this you understand the example.", rfc2047.decode("=?ISO8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?==?ISO8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=", !relaxed, true).toString());
            assertEquals("Foo bar æøå _☺", rfc2047.decode("Foo bar =?utf-8?Q?=C3=A6=C3=B8=C3=A5?= =?utf-8?Q?_=E2=98=BA?=", !relaxed, true).toString());
            assertEquals("¡Hola, señor!", rfc2047.decode("=?iso-8859-1?Q?=A1?=Hola, se=?iso-8859-1?Q?=F1?=or!", !relaxed, true).toString());
            assertEquals("Patrik_Fältström", rfc2047.decode("=?ISO8859-1?Q?Patrik_F=E4ltstr=F6m?=", !relaxed, true).toString());
            assertEquals("םולש ןב ילטפנ", rfc2047.decode("=?ISO8859-8?b?7eXs+SDv4SDp7Oj08A==?=", !relaxed, true).toString());
            assertEquals("Keld_Jørn_Simonsen", rfc2047.decode("=?ISO8859-1?Q?Keld_J=F8rn_Simonsen?=", !relaxed, true).toString());
            relaxed = !relaxed;
        } while (relaxed == false);
    }
}
