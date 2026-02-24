/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.remoteapi.tests;

import org.appwork.remoteapi.ContentSecurityHeader;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;

@TestDependency({ "org.appwork.remoteapi.ContentSecurityHeader" })
public class ContentSecurityHeaderTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        testRestrictiveDefault();
        testOverrideDefaultSource();
        testConnectSrcDirective();
        testIgnoreEmptyValues();
    }

    private void testRestrictiveDefault() throws Exception {
        final ContentSecurityHeader csp = new ContentSecurityHeader();
        final String header = csp.toHeaderString();
        assertTrue(header != null, "Default CSP header must not be null");
        assertTrue(header.contains("default-src 'none'"), "Default CSP must contain maximum restrictive default-src 'none': " + header);
    }

    private void testOverrideDefaultSource() throws Exception {
        final ContentSecurityHeader csp = new ContentSecurityHeader();
        csp.addDefaultSrc("'self'");
        final String header = csp.toHeaderString();
        assertTrue(header.contains("default-src"), "Header must contain default-src");
        assertTrue(header.contains("'self'"), "Header must contain explicitly configured default source");
        assertFalse(header.contains("'none'"), "Header must remove 'none' once a less restrictive default-src was configured: " + header);
    }

    private void testConnectSrcDirective() throws Exception {
        final ContentSecurityHeader csp = new ContentSecurityHeader();
        csp.addConnectSrc("'self'");
        csp.addConnectSrc("https://api.example.test");
        final String header = csp.toHeaderString();
        assertTrue(header.contains("connect-src"), "Header must contain connect-src directive: " + header);
        assertTrue(header.contains("https://api.example.test"), "Header must contain custom connect-src host: " + header);
    }

    private void testIgnoreEmptyValues() throws Exception {
        final ContentSecurityHeader csp = new ContentSecurityHeader();
        csp.addConnectSrc(null);
        csp.addConnectSrc("");
        csp.addConnectSrc("   ");
        csp.addScriptSrc("");
        csp.addStyleSrc(null);
        final String header = csp.toHeaderString();
        assertFalse(header.contains("connect-src  "), "connect-src must not contain empty tokens");
    }
}
